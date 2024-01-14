---
title:                "Bash: Enviando una solicitud http."
simple_title:         "Enviando una solicitud http."
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Por qué

Enviar solicitudes HTTP es una parte fundamental de la programación de Bash, ya que permite a los usuarios interactuar con redes y servidores externos. Esto es especialmente útil cuando se trabaja con aplicaciones web o se necesita acceder a datos externos.

## Cómo hacerlo

Para enviar una solicitud HTTP en Bash, se utiliza el comando `curl`. Por ejemplo, para obtener la página web de Google, se puede escribir lo siguiente en la terminal:

```bash
curl https://www.google.com
```

Esto enviará una solicitud GET a la URL especificada y devolverá el contenido de la página en la terminal. Si se quiere guardar el contenido en un archivo, se puede usar la opción `-o` seguido del nombre del archivo:

```bash
curl -o google.html https://www.google.com
```

También se pueden enviar solicitudes POST con `curl` incluyendo la opción `-X POST` seguida de los datos a enviar entre comillas simples:

```bash
curl -X POST -d '{"username":"ejemplo", "password":"contraseña"}' https://www.ejemplo.com/login
```

La respuesta será el resultado de la solicitud POST en la terminal. Se pueden usar muchas otras opciones y personalizar la solicitud según sea necesario.

## Profundizando

Enviar una solicitud HTTP en Bash puede ser más complejo que simplemente usar el comando `curl`. Es posible que se necesite agregar encabezados, autenticación u otros parámetros a la solicitud. Para hacerlo, se pueden utilizar variables y concatenarlas en la solicitud. Por ejemplo, para agregar un encabezado de autenticación con un token de acceso, se puede escribir lo siguiente:

```bash
token="mi_token_de_acceso"
curl -H "Authorization: Bearer $token" https://www.ejemplo.com/api/data
```

Además, se pueden usar lenguajes como Awk para procesar la salida de una solicitud y extraer datos específicos de ella.

## Ver también

- [Documentación de cURL](https://curl.haxx.se/docs/)
- [Tutorial de cURL para principiantes](https://www.codecademy.com/articles/curl)
- [Tutorial de Bash para principiantes](https://www.codecademy.com/learn/learn-the-command-line)