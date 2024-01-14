---
title:                "Fish Shell: Imprimiendo la salida de depuración"
simple_title:         "Imprimiendo la salida de depuración"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por qué: Imprimiendo salida de depuración

Imprimir la salida de depuración es una herramienta importante para los programadores que desean entender mejor lo que está sucediendo en el código. Puede ser útil ver los valores de variables o verificar si las condiciones de un bucle se están cumpliendo correctamente. Además, imprimir la salida de depuración también puede ayudar a detectar errores y solucionar problemas en el código.

## Cómo: Ejemplos de código en Fish Shell

Para imprimir la salida de depuración en Fish Shell, se puede utilizar el comando `echo`. Por ejemplo, si queremos imprimir el valor de una variable `contador`, podemos hacer lo siguiente:

```
Fish Shell
echo $contador
```

Esto imprimirá el valor actual de la variable `contador` en la terminal. También es posible imprimir mensajes personalizados junto con la salida de depuración, como se muestra en el siguiente ejemplo:

```
Fish Shell
set variable "Hola"
echo "El valor actual de la variable es: $variable"
```

Esto imprimirá en la terminal: `El valor actual de la variable es: Hola`.

## Profundizando: Más información sobre la impresión de la salida de depuración

Además de utilizar el comando `echo` para imprimir la salida de depuración, también se puede utilizar la variable `status` para imprimir información de estado útil, como el código de salida de un comando. Por ejemplo:

```
Fish Shell
ls /carpeta_inexistente
echo $status
```

Esto imprimirá en la terminal: `1` ya que el comando `ls` fallará al no poder encontrar la carpeta especificada y el código de salida por defecto para un error es `1`.

Otro punto importante a tener en cuenta es que es posible deshabilitar la impresión de salida de depuración con el comando `debug off` y volver a habilitarla con `debug on`.

## Ver también
- [Documentación de Fish Shell](https://fishshell.com/docs/current/index.html)
- [Tutorial de Fish Shell](https://fishshell.com/docs/2.0/tutorial.html)
- [Comandos útiles de Fish Shell](https://fishshell.com/docs/current/commands.html)