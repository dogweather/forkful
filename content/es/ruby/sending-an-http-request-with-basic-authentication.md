---
title:                "Ruby: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez has tenido la necesidad de conectarte a una API o un servidor remoto para obtener información o realizar una acción? En ese caso, es probable que hayas utilizado una solicitud HTTP con autenticación básica como método de autenticación. En esta publicación, aprenderemos por qué y cómo enviar una solicitud HTTP con autenticación básica utilizando Ruby.

## Cómo hacerlo

Primero, importamos la librería `net/http` para poder utilizar sus métodos y funciones.

```Ruby
require 'net/http'
```

Luego, definimos la URL del servidor al que nos conectaremos y el endpoint que utilizaremos en nuestra solicitud.

```Ruby
url = URI.parse("https://www.ejemploapi.com")
endpoint = "/users/1"
```

A continuación, creamos un objeto `Net::HTTP` con nuestra URL como parámetro.

```Ruby
http = Net::HTTP.new(url.host, url.port)
```

Luego, definimos los encabezados de nuestra solicitud, incluyendo la autorización básica con nuestras credenciales.

```Ruby
headers = {
  'Authorization': 'Basic YWxhZGRpbjpvcGVuc2VzYW1l'
}
```

Después, creamos un objeto `Net::HTTP::Get` con el endpoint que queremos acceder como parámetro.

```Ruby
request = Net::HTTP::Get.new(endpoint)
```

Finalmente, enviamos la solicitud utilizando el método `Net::HTTP#request` y guardamos la respuesta en una variable.

```Ruby
response = http.request(request)
```

El objeto `response` ahora contiene la información devuelta por la solicitud. Vamos a imprimir el código de estado y el cuerpo de la respuesta utilizando `puts`.

```Ruby
puts "Código de estado: #{response.code}"
puts "Cuerpo de la respuesta: #{response.body}"
```

El código de estado debería ser `200` si la solicitud fue exitosa y el cuerpo de la respuesta debería contener los datos que solicitamos del servidor.

## Profundizando

Envía una solicitud HTTP utilizando el método `Net::HTTP::Post` para crear un nuevo usuario en el servidor. También puedes experimentar con diferentes métodos como `Net::HTTP::Put` o `Net::HTTP::Delete` para actualizar o eliminar datos del servidor.

¡Eso es todo! Con esto, has aprendido cómo enviar una solicitud HTTP con autenticación básica utilizando Ruby. Espero que esto te sea útil en tu próximo proyecto.

## Ver también

- [Documentación de Net::HTTP en la guía oficial de Ruby](https://ruby-doc.org/stdlib-2.7.1/libdoc/net/http/rdoc/Net/HTTP.html)
- [Tutorial de autenticación básica en Ruby](https://www.rubyguides.com/2018/11/https-with-net-http/)
- [Ejemplos de código en GitHub para enviar solicitudes HTTP en Ruby](https://github.com/adafruit/adafruit- io-ruby/blob/master/examples/http_requests.rb)