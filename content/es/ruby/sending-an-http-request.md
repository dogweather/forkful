---
title:                "Enviando una solicitud http"
html_title:           "Bash: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Enviar una solicitud HTTP es decirle a un servidor que queremos realizar una operación en un recurso específico que aloja. Lo hacemos para interactuar con servicios web y extraer, enviar o actualizar datos.

## Cómo hacerlo: 

```Ruby
require 'net/http'
require 'uri'

uri = URI.parse("http://ejemploapi.com/recursos")
respuesta = Net::HTTP.get_response(uri)

puts respuesta.body
```

Esto enviará una solicitud GET a la dirección URL de la API y luego imprimirá la respuesta.

## Inmersión Profunda: 

Enviar solicitudes HTTP comenzó con el nacimiento de la web para recuperar y enviar datos a través de la red. En Ruby, la biblioteca Net::HTTP es la manera clásica de hacerlo.

Existen alternativas como 'rest-client' y 'HTTParty' que proporcionan interfaces más amigables.

En cuanto a los detalles de implementación, Net::HTTP en Ruby abre una conexión TCP/IP al servidor y luego envía una solicitud HTTP que obedece al protocolo HTTP. La respuesta es analizada y tallada en un objeto de respuesta Ruby.

## Ver También: 

1. Biblioteca Net::HTTP [Documentación Oficial](https://ruby-doc.org/stdlib-3.1.0/libdoc/net/http/rdoc/Net/HTTP.html)
2. Biblioteca 'rest-client' [Documentación Oficial](https://www.rubydoc.info/gems/rest-client/2.1.0)
3. Biblioteca 'HTTParty' [Documentación Oficial](https://www.rubydoc.info/github/jnunemaker/httparty)