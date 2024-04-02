---
date: 2024-01-20 18:01:59.630357-07:00
description: "Enviar una solicitud HTTP con autenticaci\xF3n b\xE1sica implica incluir\
  \ credenciales de usuario y contrase\xF1a en las cabeceras de una petici\xF3n HTTP\
  \ para acceder\u2026"
lastmod: '2024-03-13T22:44:59.035585-06:00'
model: gpt-4-1106-preview
summary: "Enviar una solicitud HTTP con autenticaci\xF3n b\xE1sica implica incluir\
  \ credenciales de usuario y contrase\xF1a en las cabeceras de una petici\xF3n HTTP\
  \ para acceder\u2026"
title: "Enviando una solicitud http con autenticaci\xF3n b\xE1sica"
weight: 45
---

## ¿Qué y Por Qué?

Enviar una solicitud HTTP con autenticación básica implica incluir credenciales de usuario y contraseña en las cabeceras de una petición HTTP para acceder a recursos protegidos. Los programadores utilizan este método para asegurar que sólo los usuarios autorizados puedan realizar ciertas acciones o acceder a cierta información en aplicaciones web.

## Cómo Hacerlo:

```Kotlin
import java.net.HttpURLConnection
import java.net.URL
import java.util.Base64

fun sendGetRequestWithBasicAuth(url: String, user: String, password: String) {
    val connection = URL(url).openConnection() as HttpURLConnection
    val credentials = "$user:$password"
    val encodedCredentials = Base64.getEncoder().encodeToString(credentials.toByteArray())
    
    connection.requestMethod = "GET"
    connection.setRequestProperty("Authorization", "Basic $encodedCredentials")
    
    val responseCode = connection.responseCode
    println("Response Code: $responseCode")
    
    val inputStream = connection.inputStream
    val response = inputStream.bufferedReader().use { it.readText() }
    println(response)
}

// Uso del método
val url = "http://ejemplo.com/api/recurso_protegido"
val user = "usuario"
val password = "contraseña"
sendGetRequestWithBasicAuth(url, user, password)
```

## Análisis Profundo

El término "autenticación básica" proviene de los primeros días de la web cuando se buscaban métodos seguros pero simples para verificar las identidades. A pesar de su simplicidad, la autenticación básica sigue siendo ampliamente utilizada, sobre todo para pruebas o aplicaciones con niveles de seguridad menos críticos. Sin embargo, existen alternativas más seguras como OAuth, JWT o la autenticación de dos factores para entornos más sensibles.

Implementar la autenticación básica es sencillo: codificas las credenciales en base64 y las pasas en la cabecera de autorización con el prefijo 'Basic'. Pero atención: la codificación base64 no es encriptación y puede ser decodificada fácilmente, lo que significa que las credenciales pueden ser interceptadas si no se usa una conexión HTTPS segura.

## Ver También

- Autenticación básica en la especificación de HTTP: [https://tools.ietf.org/html/rfc7617](https://tools.ietf.org/html/rfc7617)
- Guía de autenticación de seguridad de Spring para aplicaciones Kotlin/Spring: [https://spring.io/guides/tutorials/spring-security-and-angular-js/](https://spring.io/guides/tutorials/spring-security-and-angular-js/)
