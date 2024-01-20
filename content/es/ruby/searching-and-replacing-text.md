---
title:                "Buscando y reemplazando texto"
html_title:           "C: Buscando y reemplazando texto"
simple_title:         "Buscando y reemplazando texto"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

La búsqueda y sustitución de texto se refiere a encontrar texto específico y reemplazarlo por otro. Esta técnica es esencial en la programación para manipular datos, automatizar tareas y corregir errores.

## Cómo Hacerlo:

En Ruby, puedes utilizar el método `.gsub` para buscar y reemplazar texto. Aquí tienes un ejemplo:

```ruby
texto = "Hola Mundo"
texto.gsub!("Mundo", "Ruby")
puts texto
```

Salida del programa:

```
Hola Ruby
```

En este ejemplo, 'Mundo' es el texto que estamos buscando y 'Ruby' es el texto con el que queremos reemplazarlo.

## Profundizando

El método `.gsub` ha sido una característica de Ruby desde sus comienzos. La funcionalidad de búsqueda y reemplazo también puede lograrse utilizando expresiones regulares para encontrar patrones más complejos en lugar de texto simple.

Otros métodos para buscar y reemplazar incluyen `.sub`, que solo reemplaza la primera aparición del texto buscado, y `.gsub!`, que realiza la sustitución en el objeto original en lugar de devolver uno nuevo.

Aquí vemos un ejemplo con `.sub`:

```ruby
texto = "quiero Ruby, Ruby es genial"
texto.sub!("Ruby", "programar en Ruby")
puts texto
```

Salida del programa:

```
quiero programar en Ruby, Ruby es genial
```

Como puedes ver, solo la primera aparición de 'Ruby' ha sido reemplazada.

## Ver También

Para más información, consulta las siguientes fuentes:

* Documentación oficial de Ruby sobre el método `.gsub`: [ruby-doc.org/core-2.7.1/String.html#method-i-gsub](https://ruby-doc.org/core-2.7.1/String.html#method-i-gsub)
* Explicación útil con ejemplos en Stack Overflow: [stackoverflow.com/questions/19445003/using-ruby-gsub-to-replace-a-string](https://stackoverflow.com/questions/19445003/using-ruby-gsub-to-replace-a-string)
* Más información y ejemplos sobre la manipulación de strings en Ruby: [www.rubyguides.com/2018/01/ruby-string-methods](https://www.rubyguides.com/2018/01/ruby-string-methods/)