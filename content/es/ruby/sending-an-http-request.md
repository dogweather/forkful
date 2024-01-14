---
title:                "Ruby: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Por qué

Enviar solicitudes HTTP es una parte fundamental de la programación en Ruby. Permite a los desarrolladores interactuar con servidores web y acceder a información de una forma sencilla y eficiente. Aprender a enviar solicitudes HTTP te ayudará a crear aplicaciones web dinámicas y completas.

## Cómo hacerlo

Para enviar una solicitud HTTP en Ruby, utilizamos la librería `net/http`. Primero, debemos requerirla en nuestro código:

```Ruby
require 'net/http'
```

Luego, podemos usar el método `get`, `post`, `put` o `delete` según el tipo de solicitud que queremos enviar. Por ejemplo, para enviar una solicitud GET a un servidor web, podemos hacer lo siguiente:

```Ruby
response = Net::HTTP.get_response('www.ejemplo.com', '/')
```

Esto enviará una solicitud a `www.ejemplo.com` con la ruta `/` y guardará la respuesta en una variable llamada `response`.

Podemos imprimir la respuesta en la consola para verla:

```Ruby
puts response.body
```

Esto nos mostrará el contenido de la página en caso de que la solicitud sea exitosa.

## Profundizando

Al enviar una solicitud HTTP, necesitamos especificar varios parámetros, como la URI, la ruta y el tipo de solicitud. También podemos enviar parámetros adicionales, como cabeceras o cuerpo de la solicitud.

La URI en una solicitud HTTP es equivalente a una dirección web. En el ejemplo anterior, la URI es `www.ejemplo.com`.

La ruta es la parte de la URI después del nombre de dominio. Por ejemplo, en `www.ejemplo.com/pagina`, la ruta sería `/pagina`.

Los tipos de solicitud más comunes son GET, POST, PUT y DELETE. Estos se utilizan para recuperar, crear, actualizar y eliminar información, respectivamente.

Además, podemos enviar cabeceras en nuestra solicitud para proporcionar más información al servidor. Podemos hacerlo pasando un hash como parámetro al método `get`, `post`, `put` o `delete`.

Por ejemplo, para enviar una solicitud con una cabecera de autenticación, podemos hacer lo siguiente:

```Ruby
response = Net::HTTP.post('www.ejemplo.com', '/usuarios', { 'Authorization' => '12345' })
```

Por último, en algunas solicitudes, necesitaremos enviar datos en el cuerpo de la solicitud. Esto se puede hacer pasando un cuerpo como string al método `post`, `put` o `delete`.

```Ruby
response = Net::HTTP.post('www.ejemplo.com', '/usuarios', 'nombre=Juan&edad=30')
```

## Ver también

- [Documentación de la librería `net/http`](https://ruby-doc.org/stdlib/libdoc/net/http/rdoc/Net/HTTP.html)
- [Tutorial de HTTP en Ruby](https://www.rubyguides.com/2018/08/ruby-send-http-request/)
- [Ejemplo práctico de envío de solicitudes HTTP en Ruby](https://medium.com/@lucarv/how-to-send-http-requests-in-ruby-3a41aadbbead)