---
title:                "Gleam: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Por qué

Enviar una solicitud HTTP es una tarea común en la programación moderna. A menudo, se utiliza para comunicarse con servidores web y obtener información o realizar acciones en línea. En Gleam, esta tarea se puede realizar de forma eficiente utilizando el módulo `gleam/http`.

## Cómo hacerlo

Primero, debemos importar el módulo `gleam/http` en nuestro archivo Gleam:

```Gleam
import gleam/http
```

Luego, podemos enviar una solicitud GET utilizando la función `get` de este módulo. Por ejemplo, si queremos obtener el contenido HTML de un sitio web, podemos hacer lo siguiente:

```Gleam
let resultado = http.get("https://www.ejemplo.com/")
```

El resultado de esta función será un tipo de dato `Result`, que puede ser `Ok` si la solicitud fue exitosa o `Error` si ocurrió algún problema. Podemos usar un `case` statement para manejar ambos casos de manera adecuada:

```Gleam
case resultado {
  Ok(respuesta) -> respuesta.body
  Error(error) -> panic(error)
}
```

En el caso de `Ok`, podemos acceder al cuerpo de la respuesta utilizando el campo `body`. Y en el caso de `Error`, simplemente imprimimos el error con la función `panic` para mostrarlo en la consola. 

También podemos enviar una solicitud POST utilizando la función `post` del módulo `gleam/http`. Supongamos que queremos enviar un formulario a un servidor y obtener una respuesta. Podemos hacer lo siguiente:

```Gleam
let cuerpo = http.request_body(
  [("nombre", "Juan"), ("apellido", "Pérez")]
)
let resultado = http.post("https://www.ejemplo.com/submit_form/", cuerpo)
```

Aquí estamos utilizando la función `request_body` para crear un cuerpo de solicitud con una lista de claves y valores. Luego, enviamos esta solicitud utilizando `post` y procedemos a manejar los resultados de la misma de la misma manera que en el ejemplo anterior.

## Profundizando

El módulo `gleam/http` ofrece muchas más funciones y opciones para enviar solicitudes HTTP. Podemos especificar encabezados, manejar cookies, autenticación y más. Para obtener más información y ejemplos, podemos consultar la documentación oficial en [https://gleam.run/modules/http.html](https://gleam.run/modules/http.html).

## Ver también

- [Documentación oficial de Gleam](https://gleam.run/)
- [Módulo HTTP de Gleam](https://gleam.run/modules/http.html)
- [Tutorial de introducción a Gleam (en español)](https://codigofacilito.com/articulos/tutorial-introductorio-gleam-lenguaje-functional)