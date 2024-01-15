---
title:                "Enviando una solicitud http"
html_title:           "Ruby: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

## ¿Por qué enviar una solicitud HTTP?

Enviar solicitudes HTTP es fundamental para interactuar con diferentes aplicaciones web y servicios en línea. Puedes enviar solicitudes a una API para obtener datos, cargar archivos a un servidor o simplemente navegar por páginas web. En resumen, enviar solicitudes HTTP es esencial para realizar cualquier actividad en línea.

## Cómo hacerlo

Para enviar una solicitud HTTP en Ruby, primero debes requerir la librería 'net/http'. Luego, puedes utilizar el método .get() para hacer una solicitud GET a una URL específica. A continuación, se muestra un ejemplo de cómo obtener los datos de un servicio de clima (weather) utilizando la API de OpenWeatherMap:

```Ruby
require 'net/http'

url = URI('https://api.openweathermap.org/data/2.5/weather?q=Madrid&APPID={API_KEY}')
response = Net::HTTP.get(url)
puts response
```

El código anterior enviará una solicitud GET a la URL proporcionada con la ciudad de Madrid y una clave de API válida para OpenWeatherMap. El resultado será un objeto de respuesta HTTP que contiene los datos del clima de Madrid en formato JSON.

## Profundizando

En la sección anterior, solo utilizamos el método .get() para enviar una solicitud GET básica. Sin embargo, existen otros métodos HTTP que se pueden utilizar para enviar diferentes tipos de solicitudes. Algunos de los más comunes son:

- GET: se utiliza para obtener datos de un servidor.
- POST: se utiliza para enviar datos al servidor, como formularios o información de usuarios.
- PUT: se utiliza para actualizar datos ya existentes en el servidor.
- DELETE: se utiliza para eliminar datos del servidor.

Además, también se pueden enviar encabezados (headers) con una solicitud HTTP para proporcionar información adicional al servidor. Por ejemplo, se pueden enviar encabezados de autenticación o de tipo de contenido. Para enviar estos encabezados, se puede utilizar el método .add_field() junto con la solicitud.

## Ver también 

Para obtener más información sobre cómo enviar solicitudes HTTP en Ruby, puedes consultar la documentación oficial de Net::HTTP en https://ruby-doc.org/stdlib-2.5.3/libdoc/net/http/rdoc/Net/HTTP.html. También puedes aprender más sobre los diferentes métodos y encabezados HTTP en el sitio web de MDN: https://developer.mozilla.org/es/docs/Web/HTTP.