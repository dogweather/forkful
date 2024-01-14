---
title:    "Fish Shell: Escribiendo en el error estándar"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Por qué

Escribir a la salida de error estándar, o stderr, es una forma de mejorar la eficiencia y la legibilidad de los programas escritos en Fish Shell. Al enviar mensajes de error y excepciones a stderr, permitimos que el programa continúe ejecutándose sin interrupciones innecesarias.

## Cómo hacerlo

Escribir a stderr en Fish Shell es sencillo y puede hacerse de varias maneras.

### Método 1: Usar el comando `echo`

El comando `echo` se utiliza para mostrar texto en la línea de comandos. Puede ser utilizado junto con el operador de redirección `>` para enviar el resultado a un archivo o una tubería. Para escribir a stderr, podemos utilizar el descriptor de archivo `2`, que representa la salida de error estándar.

```Fish Shell
echo "Este es un mensaje de error" >[2=1]
```

En este ejemplo, estamos redirigiendo la salida de `echo` al descriptor de archivo `2`, que luego se envía a stderr. También podemos utilizar el atajo `&` para especificar el descriptor de archivo en lugar del nombre completo.

```Fish Shell
echo "Este es un mensaje de error" >&2
```

### Método 2: Usar funciones Fish Shell

Fish Shell también proporciona funciones integradas para escribir a stderr, como `echo_err` y `printf_err`. Estas funciones se comportan de manera similar a sus contrapartes `echo` y `printf`, pero envían la salida a stderr en lugar de stdout.

```Fish Shell
echo_err "Este es un mensaje de error"
```

```Fish Shell
printf_err "La respuesta es %d" 42
```

## Profundizando

Si bien enviar mensajes de error a stderr puede mejorar la eficiencia y la legibilidad de nuestros programas, también es importante tener en cuenta que el uso excesivo de mensajes de error puede obstaculizar la experiencia del usuario. Por lo tanto, es importante encontrar un equilibrio entre la cantidad y la relevancia de los mensajes de error.

También es importante tener en cuenta que stderr es solo uno de los tres canales de salida (los otros son stdout y stdout interno). Al escribir a stderr, es posible que estemos sobrescribiendo mensajes importantes que se envían a stdout o stdout interno. Por lo tanto, es importante comprender cómo funcionan estos canales y utilizarlos adecuadamente en nuestros programas.

## Ver también

- [Documentación oficial de Fish Shell sobre redireccionamientos (en inglés)](https://fishshell.com/docs/current/cmds/redirection.html)
- [Página de manual de Fish Shell sobre el descriptor de archivo `2` (en inglés)](https://fishshell.com/docs/current/index.html#fd)
- [Tutorial de Fish Shell en español (en español)](https://github.com/jorgebucaran/fish-shell-cookbook/blob/master/source/es/Introduction.md)