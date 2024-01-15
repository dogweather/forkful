---
title:                "Utilizando expresiones regulares"
html_title:           "Fish Shell: Utilizando expresiones regulares"
simple_title:         "Utilizando expresiones regulares"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por qué

Si eres un programador que trabaja en la línea de comandos, es probable que hayas utilizado regular expressions (expresiones regulares) antes. Estas poderosas herramientas te permiten realizar búsquedas y manipular cadenas de texto de una manera muy precisa y eficiente. Aprender a utilizar las expresiones regulares en Fish Shell te ahorrará tiempo y te hará más productivo en tus tareas diarias.

## Cómo hacerlo

Utilizar expresiones regulares en Fish Shell es sencillo. Simplemente sigue estos pasos:

1. Abre tu terminal y ejecuta `fish` para activar Fish Shell.
2. Escribe `man fish` para acceder al manual de Fish Shell y busca la sección sobre expresiones regulares.
3. Ahora que tienes una idea de cómo funcionan las expresiones regulares en Fish Shell, ¡es hora de probarlas! En los bloques de código siguientes, te mostraré algunos ejemplos de cómo utilizarlas.

```Fish Shell
# Buscar el patrón "hello" en una cadena de texto
echo "Hola amigos, hello world" | grep -o "hello"

# Resultado: hello
```

```Fish Shell
# Reemplazar el primer "hello" por "hola"
echo "Hola amigos, hello world" | sed "s/hello/hola/"

# Resultado: Hola amigos, hola world
```

```Fish Shell
# Reemplazar todas las vocales por "X"
echo "Este es un texto de ejemplo." | sed "s/[aeiou]/X/g"

# Resultado: XstX Xs Xn tXxtX dX XjXmplX.
```

## Más información

Ahora, profundicemos un poco más en el uso de las expresiones regulares en Fish Shell. Una de las características únicas de Fish Shell es que utiliza el lenguaje de patrones de Unix (UPattern) en lugar del lenguaje de patrones de texto comúnmente utilizado en otros shells. Esto significa que las expresiones regulares en Fish Shell tienen una sintaxis ligeramente diferente.

Algunas cosas a tener en cuenta al utilizar expresiones regulares en Fish Shell:

- Los metacaracteres comodín como `*` y `?` no funcionan en expresiones regulares. En su lugar, se utilizan los metacaracteres `+` y `.`.
- No es necesario utilizar comillas en las expresiones regulares.
- Puedes utilizar `[[:alpha:]]` para hacer coincidir letras y `[[:digit:]]` para hacer coincidir números.
- Puedes utilizar `$1`, `$2`, etc. para referirte a los grupos de captura en la sustitución de texto.

¡Experimenta con diferentes patrones y verás lo poderosas que son las expresiones regulares en Fish Shell!

## Ver también

- [Página de manual de Fish Shell - Sección de expresiones regulares](https://fishshell.com/docs/current/cmds/man.html#regexp-regular-expression-syntax)
- [Página de manual de Unix - Sección sobre el lenguaje de patrones](http://man7.org/linux/man-pages/man7/regex.7.html)
- [Guía de expresiones regulares en Computer Hope](https://www.computerhope.com/unix/regex.htm)