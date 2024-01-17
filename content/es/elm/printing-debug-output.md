---
title:                "Imprimiendo salida de depuración"
html_title:           "Elm: Imprimiendo salida de depuración"
simple_title:         "Imprimiendo salida de depuración"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Imprimir mensajes de depuración es una técnica utilizada por los programadores para obtener información sobre el funcionamiento de su código. Es una forma de verificar si el código está realizando correctamente las tareas deseadas y también puede ayudar a identificar errores y problemas en el mismo.

## Cómo hacerlo:
Hay diferentes formas de imprimir mensajes de depuración en Elm, pero la más común es utilizando la función `Debug.log` que acepta dos argumentos: una etiqueta para el mensaje y el valor que se desea imprimir. Por ejemplo, si queremos imprimir el valor de una variable `num`, podemos escribir:

```Elm
Debug.log "Número:" num
```
El mensaje resultante se mostrará en la consola del navegador, mostrando la etiqueta y el valor correspondiente. Aquí puedes ver un ejemplo de código y su correspondiente mensaje de depuración:

```Elm
x = 5
Debug.log "Valor de x:" x
```
Mensaje de depuración:
```
Valor de x: 5
```

## Profundizando:
Imprimir mensajes de depuración no es una técnica nueva, ya que ha sido utilizada por mucho tiempo por los programadores. Sin embargo, es importante tener en cuenta que su uso debe ser limitado y solo se debe utilizar para propósitos de prueba o durante el proceso de depuración de un código.

En Elm, también existe la función `Debug.todo` que se utiliza como marcador cuando se sabe que una parte del código aún no está completa. Esta función devuelve un mensaje de advertencia en la consola del navegador cuando se ejecuta, lo cual puede ser útil para recordar qué partes del código aún necesitan ser trabajadas.

Otra alternativa al uso de `Debug.log` es utilizar el depurador integrado de Elm, que permite rastrear y examinar el estado de las variables durante la ejecución del código. Sin embargo, el depurador solo está disponible para aplicaciones en modo de desarrollo.

## Ver También:
Puedes obtener más información sobre el uso de mensajes de depuración en la documentación oficial de Elm: [Debugging in Elm](https://guide.elm-lang.org/debugging/). También puedes encontrar recursos en línea como tutoriales y videos que te ayudarán a comprender mejor cómo utilizar esta técnica en tus proyectos.