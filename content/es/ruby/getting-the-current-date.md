---
title:                "Ruby: Obteniendo la fecha actual"
programming_language: "Ruby"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por qué

Si eres un programador de Ruby, probablemente sepas lo importante que es trabajar con fechas y horas en cualquier aplicación. Ya sea para mostrar la fecha actual en un sitio web o para programar tareas con una fecha específica, obtener la fecha actual es una habilidad esencial que todo programador debe tener en su arsenal. En esta publicación del blog, te mostraremos cómo obtener la fecha actual en Ruby de manera simple y efectiva.

## Cómo hacerlo

En Ruby, hay varias formas de obtener la fecha actual. La forma más común es utilizando el método `Time.now`, que devuelve la fecha y hora actuales en formato de objeto de tiempo. Veamos un ejemplo de cómo usar este método y su salida:

```Ruby
puts Time.now
```

La salida de este código sería algo como esto: 2020-04-29 05:43:22 +0000.

También puedes optar por obtener solo la fecha o la hora actual utilizando los métodos `Time.now.strftime`, que te permite dar formato a la fecha y hora con el uso de especificadores de formato. Por ejemplo, si solo quisieras obtener el mes y el día actual en formato de texto, podrías usar la siguiente línea de código:

```ruby
puts Time.now.strftime("%B %d")
```

La salida sería algo como esto: April 29.

## Profundizando

Ahora que sabes cómo obtener la fecha actual en Ruby, es importante entender cómo funciona el objeto de tiempo y los métodos que puedes utilizar para manipular esta información. El método `now` devuelve un objeto de tiempo que contiene la fecha y hora actuales. Puedes acceder a diferentes partes de este objeto, como el año, mes, día, hora, minuto y segundo, utilizando los métodos correspondientes, como `year`, `month`, `day`, etc. Además, también puedes realizar operaciones matemáticas con el objeto de tiempo, como sumar o restar segundos, minutos, horas, días o incluso años.

Un consejo útil es que siempre utilices la zona horaria universal (UTC) al trabajar con fechas y horas en Ruby. Esto asegurará que tus fechas sean precisas y no se vean afectadas por la zona horaria de tu ubicación.

## Ver también

- [Documentación oficial de Ruby sobre Time](https://ruby-doc.org/core-2.7.1/Time.html)
- [Un artículo sobre la importancia de trabajar con zona horaria UTC en Ruby](https://blog.pragmaticengineer.com/ruby-time/)
- [Un tutorial de Ruby sobre cómo formatear fechas](https://www.digitalocean.com/community/tutorials/how-to-format-date-and-time-in-ruby)