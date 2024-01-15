---
title:                "Enviando una solicitud http"
html_title:           "Bash: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Por qué 

Enviar solicitudes HTTP es una habilidad importante para la programación de Bash, ya que permite interactuar con diferentes servidores y obtener información o realizar acciones a través de Internet. Además, es una herramienta útil para automatizar tareas en el desarrollo web.

## Cómo 

Para enviar una solicitud HTTP en Bash, utilizamos el comando `curl` seguido de la URL del servidor y cualquier parámetro adicional que necesitemos. Por ejemplo: 

```Bash
curl www.ejemplo.com
```

Esto enviará una solicitud GET al servidor y mostrará la respuesta en la consola. También podemos incluir parámetros adicionales, como el método de solicitud, el tipo de contenido y los datos que queremos enviar, entre otros.

```Bash
curl -X POST -H "Content-Type: application/json" -d '{"nombre": "Juan", "apellido": "Perez"}' www.ejemplo.com/api/usuarios
```

Esto enviará una solicitud POST con datos en formato JSON al servidor y recibiremos una respuesta en la que se puede ver si la acción se realizó exitosamente. 

## Profundizando 

Ahora que conocemos la sintaxis básica de la solicitud HTTP en Bash, podemos explorar más opciones y parámetros disponibles. 

Para especificar el método de solicitud, utilizamos la opción `-X` seguido del método deseado, como `GET`, `POST`, `PUT`, `PATCH` o `DELETE`. 

Podemos especificar los encabezados de la solicitud utilizando la opción `-H` y pasar el nombre y el valor del encabezado que deseamos agregar. Por ejemplo, `-H "Content-Type: application/json"` establecerá el tipo de contenido de la solicitud como JSON. 

Otra opción útil es la opción `-i`, que nos mostrará tanto la información de los encabezados de la respuesta como el cuerpo. Esto es especialmente útil para depurar y comprender mejor la respuesta del servidor. 

Finalmente, podemos utilizar la opción `-o` seguida de un nombre de archivo para guardar la respuesta en un archivo en lugar de mostrarla en la consola. Esto puede ser útil cuando se trabaja con grandes cantidades de datos. 

## Ver también 

- [Documentación oficial de `curl`](https://curl.se/docs/manpage.html)
- [Ejemplos de uso de `curl`](https://www.baeldung.com/curl-rest)