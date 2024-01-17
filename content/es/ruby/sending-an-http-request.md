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

# ¿Qué y por qué?
Hacer una solicitud HTTP significa enviar una solicitud desde una aplicación o programa a un servidor. Los programadores envían solicitudes HTTP para obtener información de un servidor, como datos de un sitio web o servicio web. Esta es una forma común de obtener y enviar datos en aplicaciones y sitios web.

# Cómo hacerlo:
Con Ruby, puedes enviar solicitudes HTTP utilizando la biblioteca 'Net::HTTP'. Primero, necesitas requerir la biblioteca en tu código con ``` require 'net/http' ```. Luego, puedes utilizar el método ```.get()``` para enviar una solicitud GET a una URL y obtener los datos devueltos por el servidor, como este ejemplo:

```
require 'net/http'
response = Net::HTTP.get(URI("https://jsonplaceholder.typicode.com/posts/1"))
puts response
```

La salida en la consola mostrará los datos en formato JSON del "post" número 1 del sitio "JSONPlaceholder". Puedes utilizar otros métodos, como ```.post()```, para enviar solicitudes con diferentes verbos HTTP.

# Profundizando:
En el pasado, los programadores utilizaban bibliotecas como 'Net::HTTP' para enviar solicitudes HTTP. Sin embargo, con la popularidad de las aplicaciones web y servicios web, surgieron nuevas bibliotecas y frameworks que facilitan el manejo de solicitudes HTTP, como 'HTTParty' y 'RestClient'. Estas bibliotecas ofrecen funciones adicionales y una sintaxis más fácil de usar. 

Para implementar una solicitud HTTP, se utiliza el Protocolo de Transferencia de Hipertexto (HTTP), que es la base de la comunicación en la World Wide Web. Este protocolo define cómo los clientes (como tu aplicación) y los servidores (como el sitio web o servicio web) deben intercambiar datos. 

# Ver también:
Puedes encontrar más información y ejemplos sobre cómo enviar solicitudes HTTP en Ruby en la documentación oficial de 'Net::HTTP': https://ruby-doc.org/stdlib-2.7.1/libdoc/net/http/rdoc/Net/HTTP.html 
También puedes explorar las bibliotecas mencionadas anteriormente, 'HTTParty' y 'RestClient', y comparar sus diferencias y ventajas.