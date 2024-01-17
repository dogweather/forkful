---
title:                "Imprimiendo salida de depuración"
html_title:           "Haskell: Imprimiendo salida de depuración"
simple_title:         "Imprimiendo salida de depuración"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

¡Hola programadores! ¿Alguna vez han tenido errores en su código y han encontrado difícil entender dónde está exactamente el problema? Bueno, ¡printing debug output puede ser la solución perfecta para ustedes! En esta breve guía, les explicaré qué es imprimir debug output, por qué los programadores lo hacen y cómo hacerlo en Haskell.

## ¿Qué y por qué?
Imprimir debug output es simplemente mostrar mensajes o información adicional en la consola mientras se está ejecutando un programa. Los programadores lo hacen para seguir el progreso del programa y detectar posibles errores o problemas en el código. Puede ser una herramienta muy útil, especialmente cuando se está depurando un programa complejo.

## Cómo:
En Haskell, imprimir debug output es muy sencillo. Simplemente usamos la función `print` para mostrar un valor en la consola. Por ejemplo:

```Haskell
print "¡Hola Mundo!" 
```

Esto imprimirá "¡Hola Mundo!" en la consola cuando se ejecute el programa. Podemos imprimir variables, expresiones o cualquier otro valor que nos ayude a entender lo que está sucediendo en nuestro código.

```Haskell
numero = 10
print (numero * 2)
```

Esto imprimirá "20" en la consola. Puedes usar `print` en cualquier lugar de tu código, incluso dentro de funciones o en ciclos. Simplemente asegúrate de importar `Debug.Trace` para poder usar la función `trace`, que nos permite imprimir información adicional en la consola.

## Profundizando:
Ahora que sabemos cómo imprimir debug output, echemos un vistazo a algunos detalles adicionales. Imprimir mensajes de debug ha sido una técnica común en la programación desde los primeros días. Aunque puede ser útil, también puede ser un poco problemático ya que puede hacer que el código sea más difícil de entender y mantener. Por lo tanto, algunos programadores prefieren usar herramientas de depuración como `GHCi` en lugar de imprimir debug output en la consola.

Pero si decides imprimir debug output, es importante tener en cuenta que puede afectar el rendimiento de tu programa. Si tienes muchos mensajes de debug o si lo haces en bucles grandes, puede ralentizar la ejecución del programa. Por lo tanto, siempre es importante tener en cuenta cuántos mensajes estás mostrando y dónde los estás colocando.

## Ver también:
- [Documentación de la función `print`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Text-Show.html#v:print)
- [Documentación de la función `trace`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Debug-Trace.html#v:trace)
- [Debugging in Haskell](https://medium.com/@LambdaFairy/debugging-in-haskell-f684060d1779)