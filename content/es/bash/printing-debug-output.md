---
title:                "Bash: Imprimiendo salida de depuración"
programming_language: "Bash"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por qué

¿Te has encontrado alguna vez con un error difícil de depurar en tu código de Bash? ¿Tienes problemas para entender por qué tu programa no está funcionando como se supone? La impresión de salidas de depuración puede ser una herramienta útil para solucionar estos problemas. En esta publicación, exploraremos por qué es importante imprimir salidas de depuración en Bash y cómo hacerlo de manera eficiente.

## Cómo hacerlo

Para imprimir salidas de depuración en Bash, puedes utilizar el comando `echo` seguido de la variable o mensaje que deseas imprimir. Por ejemplo:

```Bash
echo "El valor de mi_variable es: $mi_variable"
```

Esta línea de código imprimirá el valor actual de la variable `mi_variable` en la consola cuando se ejecute el programa. También puedes utilizar el comando `printf` para imprimir salidas más complejas con formato específico. Por ejemplo:

```Bash
printf "El resultado es %d y la suma de %d y %d es %d\n" $resultado $num1 $num2 $(($num1 + $num2))
```

Esto imprimirá en la consola un mensaje con el resultado de una operación matemática utilizando las variables `resultado`, `num1` y `num2`.

## Profundizando

Aunque imprimir salidas de depuración puede parecer una tarea simple, puede ayudarte a identificar y solucionar errores más rápidamente. Es importante seleccionar cuidadosamente qué variables y mensajes imprimir para no sobrecargar la consola con información innecesaria. También puedes utilizar diferentes niveles de depuración, como `DEBUG`, `INFO` y `ERROR`, para imprimir salidas específicas según la importancia del mensaje.

Otra técnica útil es utilizar un archivo de registro (log) para guardar las salidas de depuración en lugar de imprimirlas en la consola. Esto puede ser especialmente útil si estás trabajando con scripts más largos o en entornos en producción, ya que puedes revisar el archivo de registro posteriormente para buscar cualquier problema.

## Ver también

- [Comandos de depuración de Bash](https://tldp.org/LDP/abs/html/debugging.html) - Una guía completa sobre cómo depurar programas en Bash.
- [El manual de `echo`](https://linux.die.net/man/1/echo) - Documentación oficial sobre el comando `echo` en Linux.
- [El manual de `printf`](https://linux.die.net/man/1/printf) - Documentación oficial sobre el comando `printf` en Linux.