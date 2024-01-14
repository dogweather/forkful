---
title:    "Fish Shell: Comprobar si existe un directorio"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Por qué

A menudo, cuando estamos escribiendo un script en Fish Shell, necesitamos verificar si un determinado directorio existe antes de continuar con nuestro código. Esto puede ser útil para asegurarnos de que estamos trabajando con la estructura de archivos adecuada o para evitar errores en nuestro código.

## Cómo hacerlo

En Fish Shell, podemos usar la función `test` para verificar si un directorio existe. Veamos un ejemplo:

```
if test -d ~/Documentos
    echo "El directorio Documentos existe"
end
```

En este código, estamos usando la opción `-d` de `test` para chequear si el directorio `~/Documentos` existe. Si es así, el mensaje "El directorio Documentos existe" se imprimirá en la terminal.

También podemos usar la función `test` directamente en una línea de código, como en este ejemplo:

```
test -d ~/Documentos && echo "El directorio Documentos existe"
```

Aquí, estamos usando el operador lógico `&&` para imprimir el mensaje solo si el directorio existe.

## Deep Dive

La función `test` también nos permite realizar otras verificaciones en un directorio, como comprobar si tiene permisos de lectura, escritura o ejecución. Por ejemplo, si queremos asegurarnos de que el usuario actual tenga permisos de escritura en un directorio antes de crear un archivo en él, podemos usar la opción `-w` de `test` en nuestra condición.

También es importante tener en cuenta que la función `test` devuelve un código de salida diferente dependiendo del resultado de la verificación. Si la condición se cumple, la salida es `0`, y si no, es `1`. Podemos utilizar estos códigos de salida en nuestras condiciones para realizar acciones específicas en caso de éxito o fallo.

## Ver también

- [Fish Shell documentation for `test` command](https://fishshell.com/docs/current/cmds/test.html)
- [Tutorial de Fish Shell en español](https://www.genbeta.com/desarrollo/fish-shell-tutorial-instalar-configurar-utilizar)