---
title:                "Enviando una solicitud http con autenticación básica"
html_title:           "Kotlin: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

¡Hola a todos! Siempre me ha gustado descubrir cosas nuevas y hoy quiero compartir una de ellas con ustedes. ¡Vamos a aprender sobre cómo enviar una solicitud HTTP con autenticación básica en Kotlin! ¿Por qué deberíamos hacer esto? ¡Sigue leyendo para saber más!

## ¿Por qué?

Existen muchas razones por las cuales podríamos querer enviar una solicitud HTTP con autenticación básica. Una de ellas podría ser para acceder a una API o servicio que requiere autorización antes de mostrar o procesar información. En general, este tipo de autenticación es una medida de seguridad importante para proteger los datos y comunicaciones en línea.

## ¿Cómo hacerlo?

Para enviar una solicitud HTTP con autenticación básica en Kotlin, primero necesitamos importar las librerías necesarias:

```Kotlin
import java.net.URL
import java.net.HttpURLConnection
import java.util.Base64
```

Luego, podemos crear una variable para almacenar nuestra URL y otra para almacenar nuestros datos de autenticación (en este caso, nombre de usuario y contraseña):

```Kotlin
val url = URL("https://api.ejemplo.com/informacion")
val username = "mi_usuario"
val password = "mi_contraseña"
```

A continuación, creamos nuestro objeto HttpURLConnection y establecemos el método de solicitud, el tipo de autorización y los datos de autenticación en los encabezados de la solicitud:

```Kotlin
val connection = url.openConnection() as HttpURLConnection
connection.requestMethod = "GET"
connection.setRequestProperty("Authorization", "Basic " + Base64.getEncoder().encodeToString("$username:$password".toByteArray()))
```

Finalmente, podemos recibir y leer la respuesta del servidor:

```Kotlin
val responseCode = connection.responseCode
if (responseCode != HttpURLConnection.HTTP_OK) {
    throw Exception("Error al realizar la solicitud HTTP")
} else {
    val response = connection.inputStream.bufferedReader().readText()
    println(response)
}
```

¡Y eso es todo! Ahora hemos enviado una solicitud HTTP con autenticación básica y recibido una respuesta del servidor.

## ¿Cómo funciona?

Básicamente, lo que estamos haciendo es crear un objeto HttpURLConnection y establecer los métodos y encabezados necesarios para realizar una solicitud con autenticación básica. Luego, el servidor verifica los datos de autenticación en el encabezado de la solicitud y si son correctos, devuelve la respuesta deseada.

## En Resumen

Enviar una solicitud HTTP con autenticación básica en Kotlin es una forma segura de acceder a información o servicios en línea. Simplemente necesitamos importar las librerías necesarias, establecer los encabezados de la solicitud con nuestros datos de autenticación y recibir la respuesta del servidor.

## Ver También

- [Official Kotlin Website](https://kotlinlang.org/)
- [Kotlin for Android: Learn the Basics](https://developer.android.com/kotlin/basics)
- [Working with HTTP in Kotlin](https://www.techotopia.com/index.php/Apache_HTTP_with_Kotlin_Tutorial)
- [Android Networking with Kotlin](https://blog.mindorks.com/android-networking-with-kotlin)