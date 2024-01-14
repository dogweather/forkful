---
title:                "Java: Escribiendo en el error estándar"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez te has preguntado por qué a veces, cuando ejecutas un programa en Java, aparecen mensajes de error rojos en la consola? Esa es la salida estándar de error, y aunque a veces puede ser frustrante verla, en realidad es una herramienta muy útil para los programadores.

## Cómo hacerlo

Escribir a la salida estándar de error en Java es bastante sencillo. Simplemente necesitas usar el objeto `System.err` en lugar del objeto `System.out`, que se utiliza para escribir a la salida estándar. Aquí hay un ejemplo de código que muestra cómo hacerlo:

```Java
System.err.println("Este es un mensaje de error");
```

La salida de este código sería un mensaje en rojo que indica un error. Esta es una forma muy útil de informar al usuario sobre cualquier problema que pueda surgir durante la ejecución del programa.

Por supuesto, en lugar de simplemente imprimir un mensaje, también puedes enviar objetos a la salida estándar de error.

```Java
String mensaje = "¡Este es otro mensaje de error!";
System.err.println(mensaje);
```

Esto imprimirá el contenido de la variable `mensaje` en la salida estándar de error.

## Profundizando

La razón principal por la que escribimos a la salida estándar de error es para identificar errores en nuestro código. Al imprimir mensajes o valores a esta salida, podemos tener una mejor comprensión de lo que está sucediendo en nuestro programa y solucionar problemas.

También es útil para separar la salida estándar de error de la salida estándar regular. De esta manera, podemos mantener la información de diagnóstico separada de la información que se mostrará al usuario.

Otra ventaja es que, a diferencia de la salida estándar regular, la salida estándar de error se imprime inmediatamente, lo que significa que no hay buffering. Esto puede ser importante cuando se trata de solucionar problemas en tiempo real.

## Ver También

- [Cómo utilizar la salida estándar en Java](https://www.delftstack.com/es/howto/java/how-to-use-standard-output-in-java/)
- [Guía básica de errores y excepciones en Java](https://www.baeldung.com/java-errors-exceptions)
- [Documentación oficial de Java sobre el objeto System](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html)