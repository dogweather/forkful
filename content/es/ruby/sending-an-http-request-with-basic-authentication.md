---
title:                "Enviando una solicitud de http con autenticación básica"
html_title:           "Ruby: Enviando una solicitud de http con autenticación básica"
simple_title:         "Enviando una solicitud de http con autenticación básica"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## ¿Por qué deberías enviar una solicitud HTTP con autenticación básica?

Enviar una solicitud HTTP con autenticación básica permite validar la identidad del usuario antes de acceder a un recurso en línea. Esto es útil para proteger datos sensibles y controlar el acceso a ciertas partes de una aplicación o sitio web.

## Cómo hacerlo

```ruby
require "net/http"
require "uri"

# Establecer la URL de la solicitud
url = URI.parse("http://example.com")

# Crear la solicitud HTTP
request = Net::HTTP::Get.new(url)

# Agregar encabezados para autenticación básica
request.basic_auth("username", "password")

# Enviar la solicitud y obtener la respuesta
response = Net::HTTP.start(url.host, url.port) do |http|
  http.request(request)
end

# Imprimir el código de estado de la respuesta y el cuerpo del mensaje
puts "Código de estado: #{response.code}"
puts "Cuerpo del mensaje: #{response.body}"
```

Este ejemplo utiliza la biblioteca estándar de Ruby "net/http" para realizar una solicitud GET a la URL dada. También se agrega un encabezado de autenticación básica a la solicitud utilizando el método "basic_auth" y se recibe una respuesta del servidor. Al imprimir el código de estado y el cuerpo del mensaje de la respuesta, podemos verificar si la solicitud fue exitosa y obtener los datos necesarios.

## Profundizando

La autenticación básica es un método simple pero efectivo de autenticación HTTP. Consiste en enviar las credenciales del usuario en texto plano a través de encabezados de solicitud. Sin embargo, esto no es seguro ya que la información se puede interceptar fácilmente. Es recomendable utilizar HTTPS junto con la autenticación básica para una mayor seguridad.

## Ver también

- [Documentación oficial de Ruby sobre Net::HTTP](https://ruby-doc.org/stdlib-2.7.2/libdoc/net/http/rdoc/Net/HTTP.html)
- [Artículo de Wikipedia sobre autenticación HTTP básica](https://es.wikipedia.org/wiki/Autenticaci%C3%B3n_HTTP_b%C3%A1sica)
- [Código fuente de la biblioteca estándar de Ruby en GitHub](https://github.com/ruby/ruby/tree/master/lib/net/http)