---
title:                "Usando una shell interactiva (REPL)"
date:                  2024-01-26T04:17:03.456985-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando una shell interactiva (REPL)"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Una shell interactiva o REPL (Bucle Leer-Evaluar-Imprimir), te permite probar código en tiempo real. Los programadores la utilizan para experimentar, depurar y aprender las sutilezas de Ruby sin necesidad de crear scripts completos.

## Cómo hacerlo:
El REPL de Ruby se llama IRB (Ruby Interactivo). Salta y prueba Ruby directamente desde tu terminal:

```Ruby
irb
2.7.0 :001 > puts "¡Hola, mundo de Ruby!"
¡Hola, mundo de Ruby!
 => nil
2.7.0 :002 > 5.times { print "¡Ruby! " }
¡Ruby! ¡Ruby! ¡Ruby! ¡Ruby! ¡Ruby!  => 5
```

## Profundización
Introducido en Ruby 1.8, IRB es un pilar para los Rubyistas. Se inspira en las shells interactivas de Lisp y Python, fusionando la experimentación con retroalimentación inmediata. Alternativas como Pry ofrecen más características como resaltado de sintaxis y un entorno de depuración más robusto. IRB por sí mismo es sencillo pero puede ser complementado con gemas como 'irbtools' para extender la funcionalidad. Cómo maneja IRB el bucle leer-evaluar-imprimir es leyendo cada línea de entrada, evaluándola como código de Ruby y luego imprimiendo el resultado, repitiendo este proceso hasta salir.

## Ver También
- [IRB de Ruby](https://ruby-doc.org/stdlib-2.7.0/libdoc/irb/rdoc/IRB.html)
- [La gema irbtools](https://github.com/janlelis/irbtools)