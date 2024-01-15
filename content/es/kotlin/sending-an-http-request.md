---
title:                "Enviando una petición http"
html_title:           "Kotlin: Enviando una petición http"
simple_title:         "Enviando una petición http"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez te has preguntado cómo tu navegador web es capaz de mostrar una página web tan rápidamente? ¡La respuesta es enviar una petición HTTP! Al enviar una petición HTTP, podemos obtener información de un servidor web y mostrarla en nuestro navegador.

## Cómo hacerlo

Para enviar una petición HTTP en Kotlin, primero necesitamos importar la biblioteca `java.net.URLConnection`. Luego, podemos usar la función `openConnection()` para crear una conexión URL y especificar la URL del servidor al que queremos enviar nuestra petición. A continuación, podemos usar los métodos `getInputStream()` y `readText()` para obtener la respuesta del servidor y mostrarla en nuestra aplicación.

```Kotlin
import java.net.URLConnection

val connection = URL("https://www.ejemplo.com").openConnection()
val input = connection.getInputStream().readText()
println(input)
```

Este código abrirá una conexión a la URL especificada y obtendrá el texto de la respuesta del servidor. Podemos imprimirlo en la consola o usarlo en nuestra aplicación según sea necesario.

## Profundizando

Existen diferentes tipos de peticiones HTTP, como GET, POST, PUT y DELETE. En Kotlin, podemos especificar el tipo de petición que queremos enviar agregando un método `setRequestMethod()` y pasando como parámetro el tipo de petición que deseamos enviar.

Además, también podemos agregar encabezados a nuestra petición HTTP utilizando el método `setRequestProperty()` y pasar como parámetros el nombre y el valor del encabezado que queremos agregar.

```Kotlin
import java.net.URLConnection

val connection = URL("https://www.ejemplo.com").openConnection()
connection.setRequestMethod("POST")
connection.setRequestProperty("Content-Type", "application/json")
val input = connection.getInputStream().readText()
println(input)
```

Con esta configuración, ahora estamos enviando una petición POST con el encabezado "Content-Type" especificado como "application/json". Esto es útil cuando estamos comunicándonos con una API que requiere un cierto tipo de formato para los datos.

## Ver también

- [Documentación oficial de Kotlin sobre Java Networking](https://kotlinlang.org/docs/reference/java-interop.html#java-networking)
- [Tutorial de Kotlin para HTTP Requests](https://www.tutorialspoint.com/kotlin/kotlin_http_requests.htm)
- [Ejemplo de código de Kotlin para enviar una petición HTTP POST](https://gist.github.com/EkeIN/46698f1f52446d82b52a40554afdab3b)