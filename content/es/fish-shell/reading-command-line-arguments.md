---
title:    "Fish Shell: Leyendo argumentos de línea de comando"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## ¿Por qué?

Si eres un programador que trabaja en Fish Shell, es posible que te hayas preguntado cómo puedes leer y utilizar los argumentos de línea de comando en tus programas. Los argumentos de línea de comando son una forma útil de proporcionar información al programa mientras se ejecuta. En este artículo, te mostraremos cómo puedes hacerlo utilizando Fish Shell. 

## Cómo hacerlo

Para leer los argumentos de línea de comando en Fish Shell, debes utilizar la variable `$argv` en tu código. Esta variable contiene todos los argumentos pasados al programa separados por espacios. Veamos un ejemplo sencillo de cómo imprimir los argumentos en la terminal:

```
fish
```

echo $argv

**Ejemplo de entrada:** `fish shell_program.fish arg1 arg2`

**Salida:** `arg1 arg2`

Como puedes ver, los argumentos se muestran como una sola cadena, pero aún se pueden acceder individualmente. Para hacer eso, puedes usar el operador de subconjunto `[]` y el índice correspondiente al argumento que quieres acceder. Por ejemplo:

```
echo $argv[1]
```

**Salida:** `arg1`

También puedes obtener la cantidad total de argumentos utilizando la variable `$#argv`. Por ejemplo, si quieres imprimir el número total de argumentos:

```
echo "El número total de argumentos es $#argv"
```

**Ejemplo de entrada:** `fish shell_program.fish arg1 arg2`

**Salida:** `El número total de argumentos es 2`

## Profundizando

Ahora que sabes cómo acceder a los argumentos de línea de comando en Fish Shell, es importante destacar algunas particularidades. Primero, si quieres que tu programa funcione correctamente con argumentos que contienen espacios, debes rodearlos con comillas. Por ejemplo:

```
fish shell_program.fish "argumento con espacio" "otro argumento"
```

Además, si quieres ver todos los argumentos en una sola línea separados por espacios, puedes usar el operador`string`. Por ejemplo:

```
string join " " $argv
```

**Ejemplo de entrada:** `fish shell_program.fish argumento1 argumento2`

**Salida:** `argumento1 argumento2`

También puedes validar si un argumento particular ha sido pasado al programa usando un condicional. Por ejemplo:

```
if test -z $argv[1]
  echo "El argumento 1 no ha sido proporcionado"
end
```

**Ejemplo de entrada:** `fish shell_program.fish`

**Salida:** `El argumento 1 no ha sido proporcionado`

## Ver también

- [Documentación oficial de Fish Shell sobre argumentos de línea de comando](https://fishshell.com/docs/current/cmds/fish.html#args)