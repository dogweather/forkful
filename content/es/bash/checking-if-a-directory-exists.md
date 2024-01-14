---
title:    "Bash: Comprobando si existe un directorio"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por qué

Comprobar si un directorio existe puede ser una tarea común en la programación Bash. Al hacerlo, podemos asegurarnos de que nuestro script no falle debido a la falta de un directorio necesario, o podemos tomar diferentes acciones dependiendo de si el directorio existe o no. En esta entrada, aprenderemos cómo realizar esta comprobación y profundizaremos en el tema.

## Cómo hacerlo

Hay varias formas de comprobar si un directorio existe en Bash. Una de ellas es utilizando el comando `test`, seguido de la opción `-d` (directorio) y el nombre del directorio que queremos comprobar. Veamos un ejemplo:

```Bash
if test -d directorio_ejemplo; then
    echo "Sí existe el directorio"
else 
    echo "El directorio no existe"
fi
```

En este caso, si el directorio llamado "directorio_ejemplo" existe, el comando `test` devolverá un valor verdadero y el mensaje "Sí existe el directorio" será impreso en la consola. Si el directorio no existe, se imprimirá el mensaje "El directorio no existe".

Otra forma de realizar la comprobación es utilizando el comando `[[`, que es una versión mejorada del comando `test`. Este comando también acepta la opción `-d` y el nombre del directorio a comprobar, pero utiliza una sintaxis ligeramente diferente:

```Bash
if [[ -d directorio_ejemplo ]]; then
    echo "Sí existe el directorio"
else 
    echo "El directorio no existe"
fi
```

En este caso, el resultado es el mismo que con el comando `test`, pero es importante destacar que la sintaxis utilizada por `[[` es más recomendada ya que es más robusta y acepta una mayor cantidad de opciones.

Finalmente, también es posible utilizar el comando `ls` con la opción `-d`, que lista sólo el directorio especificado sin mostrar los archivos contenidos en él. Si el directorio no existe, el comando `ls` devolverá un error y podemos aprovechar esto para imprimir el mensaje correspondiente:

```Bash
if ls -d directorio_ejemplo >/dev/null 2>&1; then
    echo "Sí existe el directorio"
else 
    echo "El directorio no existe"
fi
```

En este caso, utilizamos `>/dev/null 2>&1` para redirigir cualquier mensaje de error a la "nada", ya que solo nos interesa el código de retorno del comando `ls`.
Tenga en cuenta que también podemos utilizar cualquiera de estos métodos dentro de una estructura `if-then-else` para tomar diferentes acciones dependiendo del resultado de la comprobación.

## Profundizando en el tema

Además de las opciones mencionadas anteriormente, existen otras formas de comprobar si un directorio existe en Bash utilizando herramientas como `grep`, `find` o incluso comparando cadenas de manera similar a lo que hicimos con `test` y `[[`.
También es importante tener en cuenta que, al comprobar si un directorio existe, a menudo también queremos asegurarnos de que tengamos permisos adecuados para acceder a él. Esto se puede lograr utilizando los comandos `test` o `[[` con la opción `-x` (ejecutable).
Por último, es importante destacar que, si bien es posible comprobar si un directorio existe, en general se recomienda simplemente intentar acceder al directorio y manejar cualquier error que pueda surgir en caso de que no exista.

## Ver También

- [Comando test en Bash](https://linux.die.net/man/1/test)
- [Comando [[ en Bash](https://www.tldp.org/LDP/abs/html/special-chars.html#BRACKETSREF) 
- [Estructuras de control en Bash](https://www.tldp.org/LDP/Bash-Beginners-Guide/html/sect_07_01.html)