---
title:                "Ruby: Descargando una página web"
simple_title:         "Descargando una página web"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## ¿Por qué descargar una página web?

Descargar una página web puede ser una tarea útil para los programadores en Ruby que necesitan acceder a cierta información de un sitio web o para realizar pruebas. También puede ser una buena manera de aprender más sobre cómo funciona el código de una página web.

## Cómo hacerlo

Para descargar una página web utilizando Ruby, podemos utilizar la gema "open-uri" y su método "open". Este método acepta una URL como argumento y devuelve un objeto "File" que podemos leer utilizando el método "readlines". Veamos un ejemplo:

```
require 'open-uri'

url = "https://www.example.com"
file = open(url)
output = file.readlines

puts output
```

Esto devolverá el código HTML de la página web en forma de array, con cada línea siendo un elemento del array. Podemos iterar sobre este array para manipular la información o simplemente imprimirla en la pantalla.

## Profundizando

La gema "open-uri" también nos permite especificar el tipo de petición que queremos hacer. Por defecto, utiliza el método GET, pero si queremos realizar una petición POST, podemos utilizar el método "open". Por ejemplo:

```
url = "https://www.example.com"
params = {username: "usuario", password: "contraseña"}
file = open(url, method: :post, params: params)
```

Esto será útil si queremos enviar información a un formulario en la página web que estamos descargando.

También podemos agregar headers a nuestra petición utilizando el método "open" y pasando un hash como argumento. Esto puede ser útil si la página web requiere ciertos headers para permitir el acceso. Por ejemplo:

```
url = "https://www.example.com"
headers = {"User-Agent" => "Navegador de Ruby"}
file = open(url, headers: headers)
```

Para obtener más información sobre la gema "open-uri" y sus opciones, puedes consultar su documentación oficial.

## Visita también

- [Documentación de la gema open-uri](https://github.com/ruby/ruby/blob/trunk/lib/open-uri.rb)
- [Métodos HTTP](https://developer.mozilla.org/es/docs/Web/HTTP/Methods)
- [Uso de headers en HTTP](https://developer.mozilla.org/es/docs/Web/HTTP/Headers)