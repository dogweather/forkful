---
date: 2024-01-26 04:09:30.774007-07:00
description: "Utilizar un depurador en Ruby otorga a los programadores un superpoder\
  \ para pausar su c\xF3digo, inspeccionar variables y avanzar a trav\xE9s de su c\xF3\
  digo l\xEDnea\u2026"
lastmod: '2024-03-13T22:44:59.595866-06:00'
model: gpt-4-0125-preview
summary: "Utilizar un depurador en Ruby otorga a los programadores un superpoder para\
  \ pausar su c\xF3digo, inspeccionar variables y avanzar a trav\xE9s de su c\xF3\
  digo l\xEDnea\u2026"
title: Usando un depurador
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Utilizar un depurador en Ruby otorga a los programadores un superpoder para pausar su código, inspeccionar variables y avanzar a través de su código línea por línea. La gente lo hace para aplastar errores, entender el flujo del código y para ver exactamente qué están haciendo sus hechizos escritos (código) cuando ocurre la magia—o no.

## Cómo hacerlo:

Ruby viene con un depurador incorporado llamado `byebug`. Primero, incluye `byebug` en tu Gemfile y ejecuta `bundle install`. Luego, sitúa `byebug` justo donde quieres que tu programa tome un respiro.

```Ruby
require 'byebug'

def calcular_magia(numero)
  byebug
  numero_magico = numero * 7
  return numero_magico
end

puts calcular_magia(6)
```

Ejecutar este script detendrá la ejecución en `byebug`, y serás lanzado a una sesión interactiva donde puedes teclear comandos como:

```
step
next
continue
var local
```

El resultado de ejemplo te daría un prompt que se ve así:

```
[2, 11] en example.rb
    2: 
    3: def calcular_magia(numero)
    4:   byebug
=>  5:   numero_magico = numero * 7
    6:   return numero_magico
    7: end
    8: 
    9: puts calcular_magia(6)
(byebug)
```

## Profundizando:

Mucho antes de `byebug`, los Rubyistas usaban `debugger` y `pry`. Este último, `pry`, es más que un depurador; es un potente REPL que también puede usarse para depurar con el punto de interrupción `binding.pry`.

Alternativas al `byebug` de Ruby incluyen `pry-byebug`, que combina `pry` con la funcionalidad de `byebug`, y `ruby-debug`, que es una gema más antigua y no se mantiene activamente.

Cuando invocas a `byebug`, el depurador suspende la ejecución de tu código y te ofrece una visión del tiempo de ejecución. Puedes ver y cambiar variables, saltar a diferentes puntos del código, e incluso ejecutar algo de código Ruby línea por línea. Es algo así como tener capacidades de viaje en el tiempo para tu código Ruby.

## Ver También:

- Repositorio de GitHub de Byebug: [https://github.com/deivid-rodriguez/byebug](https://github.com/deivid-rodriguez/byebug)
- Documentación de Pry: [https://github.com/pry/pry](https://github.com/pry/pry)
- Una Guía para Depurar Aplicaciones Rails: [https://guides.rubyonrails.org/debugging_rails_applications.html](https://guides.rubyonrails.org/debugging_rails_applications.html)
