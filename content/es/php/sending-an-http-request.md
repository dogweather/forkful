---
title:                "Enviando una solicitud http"
html_title:           "PHP: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/sending-an-http-request.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Enviar una solicitud HTTP significa enviar una petición a un servidor web en busca de información o para realizar una acción. Los programadores realizan solicitudes HTTP para interactuar con otros servicios web o para obtener datos en tiempo real.

## Cómo:

```PHP
// Usando la función file_get_contents para realizar una solicitud GET a una URL específica

$url = "https://miweb.com/api/users";
$data = file_get_contents($url);

// El resultado de la solicitud se almacenará en la variable $data y se puede imprimir o manejar de acuerdo a las necesidades del programador

echo $data;
```

La salida de este ejemplo puede ser un archivo JSON con información de los usuarios de una determinada página web.

## Profundizando

Para entender mejor cómo funcionan las solicitudes HTTP, es importante tener en cuenta su contexto histórico. Originalmente, el protocolo HTTP nació en 1989 con el objetivo de intercambiar información entre sistemas informáticos. Sin embargo, en la actualidad, su uso más común es para la comunicación entre el navegador y el servidor web.

Existen diversas alternativas para realizar solicitudes HTTP en PHP, como la función `curl` y la extensión `http`. Sin embargo, la función `file_get_contents` es la opción más sencilla y ampliamente utilizada para fines básicos.

Es importante tener en cuenta que la realización de solicitudes HTTP implica una comunicación entre diferentes sistemas, por lo que es necesario estar familiarizados con los conceptos de autenticación, encabezados y códigos de respuesta.

## Ver también

- La documentación oficial de PHP sobre la función `file_get_contents`: https://www.php.net/manual/en/function.file-get-contents.php
- Un tutorial detallado en español sobre cómo realizar solicitudes HTTP en PHP: https://diego.com.es/solicitudes-http-en-php