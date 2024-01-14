---
title:                "Clojure: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Por qué

Existen varias razones por las cuales un programador podría querer enviar una solicitud HTTP en Clojure. Una de las más comunes es interactuar con una API externa para obtener datos o realizar acciones en una aplicación web.

## Cómo hacerlo

Para enviar una solicitud HTTP en Clojure, podemos utilizar la biblioteca "clj-http". Primero, necesitamos importar la biblioteca en nuestro proyecto:

```
Clojure (use 'clj-http.client)
```

Luego, podemos utilizar la función "GET" para enviar una solicitud GET a una URL específica y obtener su respuesta:

```
Clojure (http/get "https://ejemplo.com")
```

Esta función devolverá una respuesta HTTP en forma de mapa, que puede ser manipulada y utilizada según sea necesario. Por ejemplo, si queremos obtener el código de estado de la respuesta, podemos hacer lo siguiente:

```
Clojure (let [response (http/get "https://ejemplo.com")]
         (:status response))
```

## Detalles técnicos

Cuando enviamos una solicitud HTTP, hay varias cosas que pueden influir en su éxito. Por ejemplo, podemos especificar encabezados adicionales en la solicitud para proporcionar información adicional al servidor. También es importante tener en cuenta que podemos enviar diferentes tipos de datos en la solicitud, como cadenas, números o mapas, y debemos asegurarnos de que la API externa que estamos utilizando pueda manejar estos datos de manera adecuada.

## Ver también

- Documentación de "clj-http": https://github.com/dakrone/clj-http
- Tutorial de Clojure para principiantes: https://www.youtube.com/watch?v=FihU5JxmnBg
- Libros de programación en Clojure: https://www.goodreads.com/list/show/110359.Clojure_books