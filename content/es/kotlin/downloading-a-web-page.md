---
title:                "Kotlin: Descargando una página web"
simple_title:         "Descargando una página web"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Por qué
¿Te has preguntado cómo funcionan las páginas web y cómo se descargan? En este artículo exploraremos el proceso de descarga de una página web utilizando Kotlin y cómo puedes aplicar este conocimiento en tu propio código.

## Cómo hacerlo
Para descargar una página web utilizando Kotlin, podemos utilizar una biblioteca externa llamada "OkHttp". Primero, necesitaremos agregar la dependencia en nuestro archivo `build.gradle`:

```Kotlin
dependencies {
    implementation("com.squareup.okhttp3:okhttp:4.9.1")
}
```

Luego, en nuestro código, podemos crear una instancia del cliente OkHttp y realizar una solicitud GET a la URL de la página web que queremos descargar:

```Kotlin
val client = OkHttpClient()

// URL de la página web a descargar
val url = "https://miwebfavorita.com"

val request = Request.Builder()
    .url(url)
    .build()

// Realizar la solicitud al servidor
val response = client.newCall(request).execute()

// Obtener el cuerpo de la respuesta (código HTML)
val html = response.body?.string()
```

Al ejecutar este código, habremos descargado el código HTML de la página web y lo almacenaremos en la variable `html`.

## Profundizando
Ahora que ya sabemos cómo descargar una página web utilizando Kotlin, podemos profundizar en el proceso. En primer lugar, el cliente OkHttp nos permite configurar opciones adicionales en nuestra solicitud, como agregar encabezados o establecer un límite de tiempo para la respuesta.

Otra cosa importante a tener en cuenta es que, al descargar páginas web, es posible que encontremos redirecciones o errores en la respuesta. En este caso, es importante manejar estos casos en nuestro código para asegurarnos de que obtenemos el contenido deseado.

## Ver también
- [Documentación de OkHttp](https://square.github.io/okhttp/)
- [Ejemplo completo en Github](https://github.com/miusuario/ejemplo-okhttp)