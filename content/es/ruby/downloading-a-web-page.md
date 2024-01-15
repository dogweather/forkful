---
title:                "Descargando una página web"
html_title:           "Ruby: Descargando una página web"
simple_title:         "Descargando una página web"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## ¿Por qué descargar una página web?

Descargar una página web puede ser útil en muchas situaciones. Puede ser para guardar una copia local de una página importante, como para tener acceso a ella cuando no hay conexión a internet. También puede ser para analizar o extraer datos de una página web en particular.

## Cómo hacerlo

Para descargar una página web en Ruby, utilizaremos la gema open-uri. Primero, debes instalar la gema en tu computadora con el siguiente comando en la terminal:

```
gem install open-uri
```

Una vez instalada, podemos usarla en nuestro código Ruby. Importamos la gema al principio de nuestro archivo con la línea:

```
require 'open-uri'
```

Luego, para descargar la página web, simplemente utilizamos el método `open` de la gema `open-uri` y le pasamos la URL de la página que queremos descargar. Por ejemplo, para descargar la página de Google, escribimos:

```
page = open("https://www.google.com")
```

Podemos guardar la página descargada en una variable, llamada `page` en este ejemplo. Luego, podemos imprimir el contenido de la página utilizando el método `read` de la variable `page`:

```
puts page.read
```

Este código imprimirá el contenido HTML de la página de Google en la terminal. También podemos guardar el contenido en un archivo utilizando el método `write` y pasándole el nombre del archivo como argumento:

```
page.write("google.html")
```

Este código creará un archivo llamado `google.html` en el directorio donde se encuentra nuestro archivo Ruby, y guardará el contenido de la página de Google en él.

## Profundizando

La gema open-uri también nos permite descargar páginas y guardarlas en diferentes formatos, como por ejemplo PDF. También nos permite autenticarnos en páginas web privadas si tenemos los datos de acceso necesarios.

Otra funcionalidad útil es el manejo de errores al descargar páginas. Podemos utilizar la estructura `begin` `rescue` para manejar posibles errores al descargar una página, como por ejemplo si la página no existe o no se puede acceder a ella. Esto nos permite controlar el flujo de nuestro programa y manejar los errores de manera adecuada.

## Ver también

- Documentación de la gema open-uri: https://github.com/ruby/open-uri
- Ejemplos de uso de open-uri: https://www.rubyguides.com/2016/09/ruby-open-uri/
- Tutorial de descarga de páginas web en Ruby: https://www.pluralsight.com/guides/download-web-pages-using-ruby