---
title:                "Enviando una solicitud http."
html_title:           "Clojure: Enviando una solicitud http."
simple_title:         "Enviando una solicitud http."
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Por qué

Los usuarios de Clojure se pueden encontrar en situaciones en las que necesitan comunicarse con un servidor utilizando el protocolo HTTP. Ya sea para obtener datos de una API o enviar información a un servicio en línea, es importante saber cómo enviar una solicitud HTTP desde un programa en Clojure.

## Cómo hacerlo

Para enviar una solicitud HTTP en Clojure, primero debes importar la librería `clojure.http.client` utilizando la función `require`.

```Clojure
(require '[clj-http.client :as client])
```

Luego, puedes utilizar la función `client/get` para enviar una solicitud `GET` o la función `client/post` para enviar una solicitud `POST`. Ambas funciones aceptan dos parámetros: la URL a la que se enviará la solicitud y un mapa con opciones adicionales.

Por ejemplo, si queremos enviar una solicitud `GET` a la URL "https://jsonplaceholder.typicode.com/todos/1", podemos hacerlo de la siguiente manera:

```Clojure
(client/get "https://jsonplaceholder.typicode.com/todos/1")
```

Esto devolverá un `hash-map` con la respuesta del servidor. En este caso, el resultado sería el siguiente:

```Clojure
{:status 200, :headers {"content-type" "application/json; charset=utf-8" "content-length" "83" "connection" "close" "vary" "Origin" "access-control-allow-methods" "GET, POST" "access-control-allow-origin" "*"}, :body "{\"userId\": 1, \"id\": 1, \"title\": \"delectus aut autem\", \"completed\": false}" :trace-redirects [] :original-uri "https://jsonplaceholder.typicode.com/todos/1"}
```

Si deseas enviar una solicitud `POST`, puedes pasar un tercer parámetro en forma de mapa con los datos que deseas enviar al servidor.

```Clojure
(client/post "https://jsonplaceholder.typicode.com/todos" {:body {:title "Mi nueva tarea" :completed false}})
```

Esto enviará una solicitud `POST` a la URL especificada con la información proporcionada en el cuerpo de la solicitud en formato JSON.

## Profundizando

La librería `clj-http` tiene muchas más opciones que puedes utilizar para personalizar tus solicitudes HTTP. Algunas de estas opciones incluyen:

- `:headers` - un mapa con encabezados adicionales que deseas incluir en la solicitud.
- `:timeout` - un número en milisegundos que determina cuánto tiempo esperar antes de abortar la solicitud.
- `:basic-auth` - un mapa con las credenciales de autenticación básica en caso de que sea necesario.
- `:multipart?` - una bandera booleana que indica si la solicitud debe ser enviada con formato `multipart` en lugar de `form-urlencoded`.

Puedes encontrar más información sobre estas opciones y otras en la documentación oficial de `clj-http`.

# Ver también
- [Documentación oficial de clj-http](https://github.com/dakrone/clj-http)
- [Tutorial de HTTP con Clojure](https://www.clojure-data.cn/doc/book-http.html)