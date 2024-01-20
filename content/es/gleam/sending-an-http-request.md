---
title:                "Enviando una solicitud http"
html_title:           "Bash: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

El envío de una solicitud HTTP es una forma de recoger información de webs o servicios externos en el internet. Los programadores hacen esto para integrar su software con otros sistemas, obtener datos y funcionalidades.

## Cómo:

Para enviar solicitudes HTTP en Gleam, primero necesitamos importar el módulo `http`.

```Gleam
import gleam/http.{get, post}
```

Supongamos que queremos obtener datos de un API.

```Gleam
let response = 
                  get("https://jsonplaceholder.typicode.com/posts")
                  |> should_log.(_)
                  |> response.body
```

El `get` hace una solicitud GET a la URL especificada. `response.body` hace referencia al cuerpo de la respuesta que hemos obtenido.

Ahora bien, supongamos que queremos enviar datos a un API mediante una solicitud POST.

```Gleam
  let response =
                  post("https://jsonplaceholder.typicode.com/posts", "{'id': 1}")
                  |> should_log.(_)
                  |> response.body;
```

El `post` hace una solicitud POST a la URL especificada y envía los datos.

## Inmersión Profunda

Enviar solicitudes HTTP es un enfoque básico y antiguo para la comunicación entre sistemas. Nacido con el surgimiento de la web, ha sido una práctica estándar durante más de dos décadas.

Aunque hay alternativas como GraphQL o gRPC, las solicitudes HTTP siguen siendo de facto para muchas tareas debido a su simplicidad y la amplia adopción.

En el Gleam, las solicitudes HTTP se basan en la biblioteca de cliente HTTP subyacente de Erlang, que es conocida por su robustez y fiabilidad.