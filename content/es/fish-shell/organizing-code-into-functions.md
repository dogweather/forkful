---
title:                "Organizando código en funciones"
date:                  2024-01-26T01:10:10.772942-07:00
model:                 gpt-4-1106-preview
simple_title:         "Organizando código en funciones"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Organizar el código en funciones se trata de agrupar partes de un script para realizar tareas específicas. Lo hacemos porque hace que el código sea más fácil de leer, probar y reutilizar — nadie quiere navegar a través de un pantano de espagueti de código.

## Cómo hacerlo:
En Fish, escribes una función con la palabra clave `function`, le das un nombre y terminas con `end`. He aquí una simple:

```fish
function hello
    echo "¡Hola, Mundo!"
end

hello
```

Salida:
```
¡Hola, Mundo!
```

Ahora, hagamos que salude a un usuario:

```fish
function greet
    set user (whoami)
    echo "¡Hey, $user!"
end

greet
```

Salida:
```
¡Hey, tu_nombre_de_usuario!
```

Para guardarla a través de las sesiones, usa `funcsave greet`.

## Análisis Detallado
Las funciones de Fish Shell son como mini-scripts — puedes meter casi cualquier cosa ahí. Históricamente, el concepto de funciones en scripts de shell ha ahorrado innumerables horas de tecleo y depuración repetitivos. A diferencia de lenguajes de programación como Python, las funciones de Shell tienen más que ver con la conveniencia que con la estructura.

Algunos shells, como Bash, usan `function` o simplemente llaves. Fish se mantiene con `function ... end` — claro y legible. Dentro de las funciones de Fish, tienes todos los lujos: parámetros, variables locales con `set -l`, e incluso puedes definir una función dentro de otra.

No necesitarás un valor de `return` porque Fish no se centra en eso; la salida de tu función es su retorno. Y si quieres funciones persistentes disponibles para sesiones futuras, recuerda `funcsave`.

## Ver También
- El tutorial de fish sobre funciones: https://fishshell.com/docs/current/tutorial.html#tut_functions
- La documentación de fish para `function`: https://fishshell.com/docs/current/cmds/function.html
- Una guía extensiva sobre cómo escribir funciones en fish: https://fishshell.com/docs/current/index.html#syntax-function
