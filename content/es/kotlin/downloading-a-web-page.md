---
title:                "Descargar una página web"
html_title:           "Kotlin: Descargar una página web"
simple_title:         "Descargar una página web"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez has querido guardar una página web para poder acceder a ella sin conexión? ¿O tal vez quieres scrappear datos de una página para analizarlos posteriormente? Para estas tareas, es necesario descargar la página web en tu dispositivo y aquí es donde entra en juego Kotlin.

## Cómo hacerlo

Para descargar una página web en Kotlin, puedes utilizar la librería `OkHTTP`. Primero, asegúrate de agregar la dependencia en tu archivo `build.gradle`:

```Kotlin
dependencies {
    implementation "com.squareup.okhttp3:okhttp:4.9.0"
}
```

Luego, en tu código, crea una instancia de `OkHttpClient`:

```Kotlin
val client = OkHttpClient()
```

A continuación, crea una instancia de `Request` con la URL de la página que deseas descargar:

```Kotlin
val url = "https://www.example.com"
val request = Request.Builder()
    .url(url)
    .build()
```

Finalmente, utiliza `client.newCall(request)` para obtener una instancia de `Call` y llama al método `execute()` para obtener la respuesta de la petición:

```Kotlin
val response = client.newCall(request).execute()
```

Puedes obtener el contenido de la página web con `response.body?.string()`. También puedes utilizar `response.code` para obtener el código de respuesta y `response.headers` para obtener los encabezados de la respuesta.

## Profundizando

Para descargar una página web de forma eficiente, es importante tener en cuenta algunos detalles. Primero, debes asegurarte de cerrar la respuesta después de utilizarla. Puedes hacerlo utilizando el método `close()` en la instancia de `ResponseBody` que obtienes de `response.body`.

Además, es importante manejar posibles errores al descargar la página web. Puedes hacerlo utilizando un bloque `try/catch` y manejando las excepciones que puedan ocurrir.

Por último, si deseas realizar más de una petición a la misma página web, es recomendable utilizar la misma instancia de `OkHttpClient` para todas las peticiones. Esto permitirá que se reutilicen las conexiones y se optimice el rendimiento de tu aplicación.

## Ver también
- [Documentación de OkHTTP](https://square.github.io/okhttp/)
- [Tutorial de Kotlin para principiantes](https://www.freecodecamp.org/news/learn-kotlin-for-beginners/)
- [Ejemplo de descarga de una página web en Kotlin](https://gist.github.com/shubhamtrivedi1/2865aa864ae90614c868)