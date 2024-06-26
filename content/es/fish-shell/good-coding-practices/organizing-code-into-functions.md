---
changelog:
- 2024-01-28, dogweather, reviewed and added links
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:01:32.943608-07:00
description: "C\xF3mo hacerlo: En Fish, escribes una funci\xF3n con la palabra clave\
  \ `function`, le das un nombre y terminas con `end`. Aqu\xED hay una simple."
lastmod: '2024-03-13T22:44:59.507179-06:00'
model: gpt-4-0125-preview
summary: "En Fish, escribes una funci\xF3n con la palabra clave `function`, le das\
  \ un nombre y terminas con `end`."
title: "Organizando el c\xF3digo en funciones"
weight: 18
---

## Cómo hacerlo:
En Fish, escribes una función con la palabra clave `function`, le das un nombre y terminas con `end`. Aquí hay una simple:

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
    echo "¡Hola, $user!"
end

greet
```

Salida:
```
¡Hola, tu_nombre_de_usuario!
```

Para guardarla a través de sesiones, usa `funcsave greet`.

## Inmersión Profunda
Las funciones de Fish Shell son como mini-scripts: puedes meter casi cualquier cosa allí. Históricamente, el concepto de funciones en los scripts de shell ha ahorrado incontables horas de tipografía y depuración repetitivas. A diferencia de lenguajes de programación como Python, las funciones Shell son más sobre conveniencia que estructura.

Algunos shells, como Bash, usan `function` o simplemente llaves directas. Fish se mantiene con `function ... end` — claro y legible. Dentro de las funciones de Fish, obtienes todos los adornos: parámetros, variables locales con `set -l`, e incluso puedes definir una función dentro de otra función.

No necesitarás un valor de `return` porque Fish no se concentra en eso; la salida de tu función es su retorno. Y si quieres funciones persistentes disponibles para futuras sesiones, recuerda `funcsave`.

## Ver También
- El tutorial de fish sobre funciones: [https://fishshell.com/docs/current/tutorial.html#tut_functions](https://fishshell.com/docs/current/tutorial.html#functions)

### Comandos de función
- [function](https://fishshell.com/docs/current/cmds/function.html) — Crear una función
- [functions](https://fishshell.com/docs/current/cmds/functions.html) — Imprimir o borrar funciones
- [funcsave](https://fishshell.com/docs/current/cmds/funcsave.html) — Guarda la definición de una función en el directorio de carga automática del usuario
- [funced](https://fishshell.com/docs/current/cmds/funced.html) — Editar una función de manera interactiva
