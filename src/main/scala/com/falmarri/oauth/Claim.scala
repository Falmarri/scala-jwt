package com.falmarri.oauth

import org.bouncycastle.crypto.digests.SHA256Digest
import org.bouncycastle.crypto.params.AsymmetricKeyParameter
import org.bouncycastle.crypto.signers.RSADigestSigner
import org.bouncycastle.crypto.util.PrivateKeyFactory
import org.bouncycastle.openssl.PEMKeyPair
import org.bouncycastle.openssl.PEMParser

import org.apache.commons.codec.binary.Base64

import com.google.common.io.BaseEncoding

import argonaut._, Argonaut._

//abstract class JWTEncodable[T <% EncodeJson[T]] {
//
//  def encode() = BaseEncoding.base64Url().encode(this.asJson.toString.getBytes)
//  
//}

object ClientCredentials {
  var header = Map("typ" -> "JWT", "alg" -> "sha256")

  
  def apply(key: String, client_id: String) = 
    new ClientCredentials(getKey(key), client_id)
  
    def apply(key: java.io.File, client_id: String) = 
    new ClientCredentials(getKey(key), client_id)
  
  def base64EncodeURLSafe(in: Array[Byte]): String = new String(Base64.encodeBase64URLSafe(in))

  def getKey(source: String): AsymmetricKeyParameter = {
    getKey(new java.io.StringReader(source))
  }

  def getKey(file: java.io.File): AsymmetricKeyParameter = {
    getKey(scala.io.Source.fromFile(file).bufferedReader())

  }

  def getKey(source: java.io.Reader): AsymmetricKeyParameter = {
    PrivateKeyFactory
      .createKey(
        new PEMParser(source)
          .readObject()
          .asInstanceOf[PEMKeyPair]
          .getPrivateKeyInfo())
  }

  def sign(pk: AsymmetricKeyParameter, s: String) = {
    val signer = new RSADigestSigner(new SHA256Digest())
    val bytes = s.getBytes()
    signer.init(true, pk)
    signer.update(bytes, 0, bytes.length)
    BaseEncoding.base64Url().encode(
      signer.generateSignature())
  }

}

class ClientCredentials(
  val key: AsymmetricKeyParameter,
  val iss: String,
  val aud: Option[String] = None) {

  val grant = "urn:ietf:params:oauth:grant-type:jwt-bearer"



  def header() = ClientCredentials.header

  def jwt(sub:Option[String] = None, scope: Seq[String] = Nil) = 
    Claim(iss, sub, scope.mkString(" "), aud, Some(0L), Some(System.currentTimeMillis() / 1000 + 3600))
  

  def sign(sub:Option[String] = None, scope: Seq[String] = Nil) = {
    jwt(sub, scope).sign(this)
  }
}

object Claim {

  implicit def JwtCodecJson: CodecJson[Claim] =
    casecodec6(Claim.apply, Claim.unapply)("iss", "sub", "scope", "aud", "iat", "exp")

}

case class JWT(header: JwtHeader, claim: Claim, sig: Option[String])

object JwtHeader {

  implicit def JwtCodecJson: CodecJson[JwtHeader] =
    casecodec2(JwtHeader.apply, JwtHeader.unapply)("typ", "alg")
}

case class JwtHeader(typ: String = "JWT", alg: String = "sha256")

case class Claim(
  iss: String,
  sub: Option[String] = None,
  scope: String = "",
  aud: Option[String] = None,
  iat: Option[Long] = None,
  exp: Option[Long] = None) {

  def sign(implicit credentials: ClientCredentials): String = {
    val signer = new RSADigestSigner(new SHA256Digest())
    val s = BaseEncoding.base64Url().encode(credentials.header.asJson.toString.getBytes) ::
      BaseEncoding.base64Url().encode(this.asJson.toString.getBytes) :: Nil
    val bytes = s mkString "." getBytes ()
    signer.init(true, credentials.key)
    signer.update(bytes, 0, bytes.length)
    s :+ BaseEncoding.base64Url().encode(
      signer.generateSignature()) mkString "."
  }

}
