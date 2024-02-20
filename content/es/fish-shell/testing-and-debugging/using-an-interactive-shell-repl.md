---
date: 2024-01-26 04:13:55.129760-07:00
description: "REPL, o Ciclo de Leer-Evaluar-Imprimir, es un entorno de programaci\xF3\
  n interactivo que toma las entradas de los usuarios, las ejecuta y devuelve el\u2026"
lastmod: 2024-02-19 22:05:18.007948
model: gpt-4-0125-preview
summary: "REPL, o Ciclo de Leer-Evaluar-Imprimir, es un entorno de programaci\xF3\
  n interactivo que toma las entradas de los usuarios, las ejecuta y devuelve el\u2026"
title: Usando una shell interactiva (REPL)
---

{{< edit_this_page >}}

## ¿Qué y por qué?
REPL, o Ciclo de Leer-Evaluar-Imprimir, es un entorno de programación interactivo que toma las entradas de los usuarios, las ejecuta y devuelve el resultado. Los programadores lo utilizan para obtener retroalimentación instantánea, depurar y experimentar rápidamente con conceptos de codificación sin el sobrecoste de compilar y ejecutar un programa completo.

## Cómo hacerlo:
En Fish, el shell interactivo es el modo predeterminado cuando lo inicias. Así es como se ve en acción:

```Fish Shell
> set color azul
> echo "El cielo es $color"
El cielo es azul
```

También puedes ejecutar funciones incorporadas y jugar con las sustituciones de comandos:

```Fish Shell
> function cheer
      echo "¡Vamos Fish $argv!"
  end
> cheer Coders
¡Vamos Fish Coders!
```

No solo definiendo funciones, puedes ejecutar fragmentos de código sobre la marcha y ver la salida instantáneamente:

```Fish Shell
> math "40 / 2"
20
```

## Profundizando
El concepto de REPLs se remonta al lenguaje de programación Lisp en los años 1960. Esta forma de programación interactiva estableció el estándar para entornos como `ipython` de Python y `irb` de Ruby. Fish continúa la tendencia con un enfoque en la amabilidad al usuario y el uso interactivo.

Fish se diferencia de otros shells como Bash en que está diseñado con la interactividad en mente desde el principio. Proporciona resaltado de sintaxis, autosugerencias y completaciones de tabulación que lo hacen poderoso para usar en un flujo de trabajo al estilo REPL. Mejor aún, tus comandos se recuerdan y son buscables, haciendo las pruebas repetidas una brisa.

Alternativas al REPL de Fish podrían ser `bash` o `zsh` cuando se combinan con extensiones como `bash-completion` o `oh-my-zsh`, pero Fish tiende a ofrecer una experiencia más rica sin configuración adicional.

## Ver también:
- Documentación de Fish: https://fishshell.com/docs/current/index.html
- Una comparación interesante de Fish frente a otros shells: https://www.slant.co/versus/2209/3686/~fish_vs_bash
- Una exploración más profunda en REPLs: https://en.wikipedia.org/wiki/Read–eval–print_loop
- Programación interactiva en Lisp, una mirada histórica: http://www.paulgraham.com/ilisp.html
