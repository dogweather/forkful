---
title:                "PHP: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/sending-an-http-request.md"
---

{{< edit_this_page >}}

## ¿Por qué enviar una solicitud HTTP?

Enviar una solicitud HTTP es esencial en la programación web. Permite a los desarrolladores comunicarse con servidores y obtener información valiosa para sus aplicaciones. Sin enviar solicitudes, no podríamos acceder a datos en línea o interactuar con otras aplicaciones.

## Como hacerlo

Para enviar una solicitud HTTP en PHP, podemos utilizar la función `file_get_contents()` junto con la función `stream_context_create()`.

Primero, necesitamos crear un contexto de flujo con `stream_context_create()`. Esto proporcionará la configuración necesaria para nuestra solicitud, como el encabezado y los datos que queremos enviar.

Luego, podemos usar `file_get_contents()` para enviar la solicitud con el URL especificado y el contexto de flujo que acabamos de crear. Esto devolverá una respuesta del servidor en forma de una cadena de texto.

Un ejemplo de cómo enviar una solicitud GET a una API y obtener la respuesta en JSON sería el siguiente:

```
$context = stream_context_create([
    'http' => [
        'method' => 'GET',
        'header' => 'Content-Type: application/json'
    ]
]);

$response = file_get_contents('https://ejemplo-api.com/endpoint', false, $context);
$json = json_decode($response);
```

En este ejemplo, creamos un contexto de flujo que especifica el método GET y el encabezado de contenido que indica que estamos enviando datos en formato JSON. Luego, usamos `file_get_contents()` para enviar la solicitud al endpoint especificado y almacenamos la respuesta en una variable. Por último, utilizamos `json_decode()` para convertir la respuesta a un formato de objeto de PHP para poder trabajar con ella.

## Profundizando

Enviar una solicitud HTTP implica una interacción entre un cliente (nuestro código) y un servidor. Durante esta interacción, podemos especificar diferentes tipos de solicitudes, como GET, POST, PUT o DELETE, y también podemos enviar datos junto con la solicitud.

Además de `file_get_contents()`, también podemos utilizar la función `curl_exec()` para enviar solicitudes HTTP en PHP. Esto nos da más control sobre la configuración y opciones de la solicitud, pero también requiere un poco más de código.

Otra cosa importante a tener en cuenta es la gestión de errores. Si la solicitud falla, es importante detectar y manejar los errores adecuadamente para que nuestra aplicación no se bloquee o produzca resultados inesperados.

## Véase también

- [Documentación oficial de PHP para `file_get_contents()`](https://www.php.net/manual/es/function.file-get-contents.php)
- [Documentación oficial de PHP para `stream_context_create()`](https://www.php.net/manual/es/function.stream-context-create.php)
- [Documentación oficial de PHP para `json_decode()`](https://www.php.net/manual/es/function.json-decode.php)
- [Documentación oficial de PHP para `curl_exec()`](https://www.php.net/manual/es/function.curl-exec.php)