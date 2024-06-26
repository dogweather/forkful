---
date: 2024-01-20 17:58:58.801202-07:00
description: "C\xF3mo Hacerlo: Ruby facilita la b\xFAsqueda y sustituci\xF3n de texto\
  \ con m\xE9todos como `gsub`. Aqu\xED un ejemplo que cambia todas las apariciones\
  \ de \"hola\" por\u2026"
lastmod: '2024-03-13T22:44:59.575403-06:00'
model: gpt-4-1106-preview
summary: "Ruby facilita la b\xFAsqueda y sustituci\xF3n de texto con m\xE9todos como\
  \ `gsub`."
title: Buscando y reemplazando texto
weight: 10
---

## Cómo Hacerlo:
Ruby facilita la búsqueda y sustitución de texto con métodos como `gsub`. Aquí un ejemplo que cambia todas las apariciones de "hola" por "adiós":

```ruby
texto = "hola, mundo! hola de nuevo!"
texto_modificado = texto.gsub("hola", "adiós")
puts texto_modificado
# => adiós, mundo! adiós de nuevo!
```

Si solo quieres cambiar la primera aparición, puedes usar `sub`:

```ruby
texto = "hola, mundo! hola de nuevo!"
texto_modificado = texto.sub("hola", "adiós")
puts texto_modificado
# => adiós, mundo! hola de nuevo!
```

Para una búsqueda más potente, puedes utilizar expresiones regulares:

```ruby
texto = "hola, mundo! hello world!"
texto_modificado = texto.gsub(/[hH]([aeiou])/, 'H\1i')
puts texto_modificado
# => Hila, mundo! Hello world!
```

## Análisis Profundo:
Buscar y reemplazar texto es un concepto tan antiguo como la edición de texto mismo. Desde comandos de terminal como `sed` en UNIX hasta la funcionalidad incorporada en editores de texto modernos, es una operación fundamental. En Ruby, `gsub` y `sub` son métodos que pertenecen a la clase `String`. `gsub` proviene de "global substitution" (sustitución global) y `sub` de "substitution" (sustitución). Al usar expresiones regulares, puedes lograr reemplazos basados en patrones complejos, no solo palabras exactas.

Alternativamente, en situaciones que no necesitan la potencia de `gsub`, puedes emplear otros métodos de `String`, como `tr` que reemplaza caracteres individuales en lugar de patrones.

```ruby
texto = "foobar"
texto_modificado = texto.tr('abc', 'xyz')
puts texto_modificado
# => fyyzxz
```

Detalles de implementación: `gsub` y `sub` devuelven una nueva instancia de `String`, dejando intacto el original. Si necesitas modificar el `String` en su lugar, `gsub!` y `sub!` existen como sus equivalentes destructivos.

## Ver También:
Aquí algunos enlaces útiles para profundizar en el tema:

- [Ruby Doc para la clase String](https://www.ruby-doc.org/core-2.7.0/String.html)
- [Tutorial sobre Expresiones Regulares en Ruby](https://www.rubyguides.com/2015/06/ruby-regex/)
- [Documentación de Ruby sobre `gsub` y `sub`](https://apidock.com/ruby/String/gsub)
