---
title:                "Enviando una solicitud http"
date:                  2024-01-20T18:00:41.773374-07:00
model:                 gpt-4-1106-preview
simple_title:         "Enviando una solicitud http"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Enviar una solicitud HTTP es el proceso de solicitar datos o enviar datos a un servidor utilizando el protocolo HTTP. Los programadores lo hacen para interactuar con APIs, servicios web o para automatizar tareas que dependen de la web.

## Cómo hacerlo:
En Ruby, puedes utilizar la gema 'net/http' para enviar solicitudes HTTP. Aquí hay un ejemplo simple de cómo realizar una solicitud GET:

```Ruby
require 'net/http'
require 'uri'

uri = URI('http://example.com')
response = Net::HTTP.get(uri)

puts response
```

Esto imprimirá la respuesta del servidor. Para una solicitud POST, el código es un poco diferente:

```Ruby
require 'net/http'
require 'uri'

uri = URI('http://example.com/api')
req = Net::HTTP::Post.new(uri, 'Content-Type' => 'application/json')
req.body = { key: 'value' }.to_json

res = Net::HTTP.start(uri.hostname, uri.port) do |http|
  http.request(req)
end

puts res.body
```

En este caso, ves cómo enviar datos JSON al servidor.

## Deep Dive
Ruby ha evolucionado mucho en cuanto a la comunicación en red desde sus inicios. Anteriormente, las opciones eran limitadas y menos intuitivas. Alternativas como 'open-uri' y gemas como 'HTTParty' y 'RestClient' también son populares para estas tareas, proporcionando interfaces más amigables y características adicionales.

Una clave en la implementación de solicitudes HTTP en Ruby es entender objetos como URI, que maneja la construcción de URLs, y la clase `Net::HTTP`, que proporciona métodos para interactuar con servidores HTTP. Es importante manejar excepciones y saber cómo trabajar con diferentes métodos HTTP (GET, POST, PUT, DELETE), cabezas y cuerpo de la solicitud, así como interpretar las respuestas del servidor.

## See Also
- La documentación de la gema 'net/http': https://ruby-doc.org/stdlib-3.1.0/libdoc/net/http/rdoc/Net/HTTP.html
- Guía de inicio rápido para 'HTTParty': https://github.com/jnunemaker/httparty
- Documentación de 'RestClient': https://github.com/rest-client/rest-client
