---
title:                "Enviando una solicitud http con autenticación básica"
html_title:           "Arduino: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Enviando una solicitud HTTP con autenticación básica en Ruby

## ¿Qué y Por Qué?

Una solicitud HTTP con autenticación básica es una forma de verificar la identidad del usuario antes de permitirle el acceso a ciertos recursos del servidor. Los programadores la usan para proteger la información sensible en sus aplicaciones web.

## ¿Cómo hacerlo?

Aquí hay un ejemplo de cómo enviar una solicitud HTTP con autenticación básica utilizando la gema `net/http` en Ruby:

```Ruby
require 'net/http'
require 'uri'

uri = URI.parse("http://miweb.com")
request = Net::HTTP::Get.new(uri)
request.basic_auth("usuario", "contraseña")

response = Net::HTTP.start(uri.hostname, uri.port) do |http|
  http.request(request)
end

puts response.body
```

Salida de la muestra:

```Ruby
"Contenido de la página web"
```
Este script realizará una solicitud GET a `miweb.com` utilizando los credenciales proporcionados (`usuario` y `contraseña`). Luego imprime el contenido del cuerpo de la respuesta al terminal.

## Profundicemos

El uso del método 'basic_auth' se remonta a los principios del protocolo HTTP, por lo que es un método ampliamente admitido para la autenticación.

Una alternativa a la autenticación básica podría ser algo como la autenticación de portador de token, que se utiliza comúnmente para las APIs de REST.

En cuanto a la implementación, `basic_auth` codifica las credenciales en base64 y las añade en el encabezado 'Authorization' de la petición HTTP. Sin embargo, es importante conocer que la autenticación básica no es segura por sí misma, ya que las credenciales no están encriptadas y pueden ser interceptadas si no se usa junto con HTTPS.

## Ver También

* [Gema de Ruby 'net/http'](https://ruby-doc.org/stdlib-2.7.1/libdoc/net/http/rdoc/Net/HTTP.html)
* [Documentación de la RFC de Autenticación HTTP](https://datatracker.ietf.org/doc/html/rfc7617)
* [Codificación base64 en Ruby](https://apidock.com/ruby/Base64/encode64)