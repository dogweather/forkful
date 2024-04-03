---
date: 2024-01-26 00:56:21.556543-07:00
description: "El manejo de errores es esperar lo inesperado en el c\xF3digo \u2014\
  \ gestionar los errores y problemas con gracia sin que se caiga la aplicaci\xF3\
  n. Los\u2026"
lastmod: '2024-03-13T22:44:59.598722-06:00'
model: gpt-4-1106-preview
summary: "El manejo de errores es esperar lo inesperado en el c\xF3digo \u2014 gestionar\
  \ los errores y problemas con gracia sin que se caiga la aplicaci\xF3n."
title: Manejo de errores
weight: 16
---

## Cómo hacerlo:
Ruby utiliza `begin`, `rescue`, `ensure` y `end` para manejar errores. Envuelves el código de riesgo entre `begin` y `end`. Si ocurre un error, se activa `rescue`.

```Ruby
begin
  # El código de riesgo va aquí.
  puts 10 / 0
rescue ZeroDivisionError => e
  puts "¡Ups! No puedes hacer eso: #{e.message}"
ensure
  puts "Esto siempre se ejecuta, haya error o no."
end
```

Salida de muestra:
```
¡Ups! No puedes hacer eso: dividido por 0
Esto siempre se ejecuta, haya error o no.
```

## Profundizando
Históricamente, el manejo de errores en los lenguajes de programación ha evolucionado significativamente, con lenguajes antiguos a menudo teniendo mecanismos rudimentarios o inexistentes. El manejo de excepciones de Ruby se inspira en lenguajes como Python y Smalltalk.

Alternativas a `begin-rescue` en Ruby incluyen usar `rescue` en definiciones de métodos o emplear `throw` y `catch` para el control de flujo no estándar, aunque no se usan para el manejo de errores típico.

Un detalle interesante: las excepciones en Ruby son objetos (instancias de la clase `Exception` y sus descendientes), así que puedes definir clases de error personalizadas y hacer más que solo registrar errores — puedes llevar un estado rico a través del programa para un manejo de errores más robusto.

## Ver también
- La documentación de Ruby sobre excepciones y manejo de errores: [ruby-doc.org](https://ruby-doc.org/core-3.1.0/doc/syntax/exceptions_rdoc.html)
- Una guía detallada sobre las mejores prácticas para el manejo de errores en Ruby: [thoughtbot.com](https://thoughtbot.com/blog/rescue-standarderror-not-exception)
