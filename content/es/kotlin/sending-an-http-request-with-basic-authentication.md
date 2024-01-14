---
title:                "Kotlin: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Por qué enviar una solicitud HTTP con autenticación básica

Enviar una solicitud HTTP con autenticación básica es una forma simple y segura de acceder a recursos protegidos en la web. Al utilizar la autenticación básica, se envía un nombre de usuario y una contraseña al servidor en la cabecera de la solicitud HTTP, lo que permite la verificación y acceso al recurso solicitado.

## Cómo hacerlo

Para enviar una solicitud HTTP con autenticación básica en Kotlin, primero debemos importar la clase Base64 desde la librería java.util.Base64. A continuación, crearemos una instancia de URL con la dirección del recurso que deseamos acceder y una instancia de HttpURLConnection con la URL creada.

Dentro del bloque de código, utilizaremos el método setRequestProperty() para añadir una cabecera de autorización a la conexión, utilizando la codificación Base64 del nombre de usuario y contraseña separados por dos puntos. Finalmente, utilizaremos el método connect() para establecer la conexión y el método getInputStream() para obtener la respuesta del servidor.

```Kotlin
import java.net.HttpURLConnection
import java.net.URL
import java.util.Base64

val username = "usuariodemo"
val password = "contrasenademo"
val resourceURL = "https://ejemplo.com/recursos/usuarios"

val url = URL(resourceURL)
val connection = url.openConnection() as HttpURLConnection
val authString = "$username:$password"

connection.setRequestProperty("Authorization", "Basic " + Base64.getEncoder().encodeToString(authString.toByteArray()))

connection.connect()

val inputStream = connection.inputStream
println(inputStream.reader().readText())
```

La salida de este código será la respuesta del servidor, que puede ser una cadena de texto o un archivo en formato JSON, XML, entre otros. Con esto, hemos enviado una solicitud HTTP con autenticación básica utilizando Kotlin.

## Profundizando más

Además de enviar una solicitud HTTP con autenticación básica de forma manual, también podemos utilizar librerías de terceros como Retrofit o OkHttp para simplificar el proceso.

Retrofit es una librería de cliente HTTP para Android y Java que nos permite realizar solicitudes HTTP de forma sencilla utilizando interfaces y anotaciones. Con Retrofit, podemos enviar solicitudes con autenticación básica utilizando el método `@Headers` y especificando la cabecera de la autorización en la anotación.

```Kotlin
interface ResourcesApi {
    @Headers("Authorization: Basic usuariodemo:contrasenademo")
    @GET("recursos/usuarios")
    fun getUsers(): Call<List<User>>
}
```

OkHttp, por otro lado, es una librería que nos permite realizar peticiones HTTP de forma eficiente y sencilla. Al igual que con Retrofit, podemos enviar una solicitud con autenticación básica utilizando el método `authenticator()` y especificando el nombre de usuario y contraseña en la misma.

```Kotlin
val client = OkHttpClient.Builder()
        .authenticator(BasicAuthenticator("usuariodemo", "contrasenademo"))
        .build()

val request = Request.Builder()
        .url("https://ejemplo.com/recursos/usuarios")
        .build()

val response = client.newCall(request).execute()
println(response.body()?.string())
```

Ahora que conocemos estas opciones, podemos elegir la que mejor se adapte a nuestras necesidades para enviar una solicitud HTTP con autenticación básica utilizando Kotlin.

## Ver también

- [Documentación Oficial de Java: Basic Authentication](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/net/HttpURLConnection.html)
- [Retrofit](https://square.github.io/retrofit/)
- [OkHttp](https://square.github.io/okhttp/)