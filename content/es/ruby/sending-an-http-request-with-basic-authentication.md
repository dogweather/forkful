---
title:                "Enviando una solicitud http con autenticación básica"
html_title:           "Ruby: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Enviar una solicitud HTTP con autenticación básica es un proceso común en la programación. Permite a los programadores acceder a recursos protegidos por contraseña al incluir las credenciales en la solicitud.

## Cómo:

```Ruby
require 'net/http'

url = URI("https://test.com")

http = Net::HTTP.new(url.host, url.port)
http.use_ssl = true

request = Net::HTTP::Get.new(url)
request.basic_auth("username", "password")

response = http.request(request)
puts response.read_body
```

Este código utiliza la biblioteca `net/http` de Ruby para crear una instancia de un objeto `Net::HTTP` y enviar una solicitud HTTP con el método `basic_auth` para agregar las credenciales necesarias.

## Deep Dive:

 La autenticación básica en HTTP es un protocolo de seguridad más antiguo que ha sido reemplazado por otras formas de autenticación más seguras, como OAuth. Sin embargo, es aún ampliamente aceptado y utilizado. 

 En lugar de enviar las credenciales con cada solicitud, se puede crear una sesión de autenticación para un usuario específico y usar un token de autenticación válido para todas las solicitudes posteriores. 

 Para implementar la autenticación básica en Ruby, también se pueden utilizar otras bibliotecas como `HTTParty` o `Faraday` que facilitan el proceso de construcción y envío de solicitudes HTTP con autenticación básica.

## Ver también:

Para obtener más información sobre la autenticación básica en HTTP y cómo se compara con otras formas de autenticación, consulte los siguientes recursos:

- [Documentación de Net::HTTP](https://ruby-doc.org/stdlib-2.7.2/libdoc/net/http/rdoc/Net/HTTP.html)
- [Tutorial de HTTP básico en Ruby](https://scotch.io/tutorials/ruby-on-rails-authentication-with-basic-http-authentication)
- [Comparación de los métodos de autenticación en HTTP](https://auth0.com/blog/historical-timeline-of-web-authentication-methods/)