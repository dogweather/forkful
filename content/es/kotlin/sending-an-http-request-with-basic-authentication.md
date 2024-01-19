---
title:                "Enviando una solicitud http con autenticación básica"
html_title:           "Arduino: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Envío de una solicitud HTTP con autenticación básica en Kotlin

## ¿Qué y por qué?

Una solicitud HTTP con autenticación básica (Basic Auth) en Kotlin es el mecanismo que permite transmitir credenciales de usuario (nombre de usuario y contraseña) en una solicitud HTTP. Los programadores lo hacen para interactuar con APIs que requieren autenticación.

## ¿Cómo hacerlo?

A continuación, mostramos cómo se puede enviar una solicitud HTTP con autenticación básica en Kotlin utilizando la biblioteca Fuel HTTP.

```kotlin
import com.github.kittinunf.fuel.httpGet

fun main() {
    val (request, response, result) = 
        "https://miapi.com".httpGet().authenticate("usuario", "contraseña").responseString()
        
    when(result) {
        is Result.Failure -> {
            val exception = result.getException()
            println(exception)
        }
        is Result.Success -> {
            val data = result.get()
            println(data)
        }
    }
}
```

Y aquí está la respuesta esperada.

```
{ "respuesta": "Éxito" }
```

## Un poco más de contexto

Históricamente, Basic Auth es uno de los métodos de autenticación más antiguos singulares por su simplicidad. Aunque se ha convertido en estándar, hoy en día es considerado una forma de autenticación más "débil". 

Como alternativa, puedes utilizar autenticación por token, OAuth u otros mecanismos de autenticación más seguros. 

Al usar Basic Auth con Fuel en Kotlin, las credenciales se convierten en una cadena en formato `usuario:contraseña`, luego se codifican en Base64 y se incluyen en el encabezado de Autorización de cada solicitud HTTP.

## Ver también

1. Kotlin Programming Language: [Web oficial](https://kotlinlang.org/)
2. Kotlin Fuel HTTP: [Repositorio de GitHub](https://github.com/kittinunf/Fuel)
3. Autenticación HTTP: [Wikipedia](https://es.wikipedia.org/wiki/Autenticaci%C3%B3n_HTTP)