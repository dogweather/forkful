---
title:    "Java: Escritura en el error estándar"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Por qué

Escribir a la salida de error estándar puede ser una tarea importante en la programación en Java. Permite mostrar mensajes de error al usuario o guardar información de errores en registros de archivo. En esta publicación explicaremos cómo realizar esta tarea de manera eficiente y efectiva.

## Cómo hacerlo

Para escribir a la salida de error estándar en Java, podemos utilizar la clase `System` y el método `err` de la siguiente manera:

```Java
System.err.println("Mensaje de error");
```

Este método imprimirá un mensaje en la salida de error estándar, que generalmente se muestra en la consola de la aplicación. También podemos utilizar la clase `PrintStream` para manejar la salida de error de manera más flexible:

```Java
PrintStream errorStream = System.err;
errorStream.println("Mensaje de error");
```

Este código también imprimirá un mensaje en la salida de error estándar, pero nos permite personalizar la forma en que se muestra, por ejemplo, utilizando el método `printf` para formatear el mensaje:

```Java
PrintStream errorStream = System.err;
errorStream.printf("El número de error es %d", 404);
```

El resultado en la consola sería: "El número de error es 404".

## Profundizando

Es importante tener en cuenta que escribir a la salida de error estándar no es lo mismo que lanzar una excepción. La salida de error es simplemente una forma de mostrar mensajes de error, mientras que una excepción permite manejar de manera más precisa y controlada cualquier problema en el código.

También puede ser útil redirigir la salida de error estándar a un archivo, especialmente en aplicaciones en producción. Esto se puede lograr utilizando la clase `FileOutputStream`:

```Java
PrintStream errorStream = new PrintStream(new FileOutputStream("filepath/error.log"));
System.setErr(errorStream);
```

Esto creará un archivo "error.log" en la ruta especificada y redirigirá la salida de error estándar a este archivo en lugar de la consola.

## Ver también

- [Documentación oficial de Java: System](https://docs.oracle.com/javase/7/docs/api/java/lang/System.html)
- [Tutorialspoint: Manejo de errores en Java](https://www.tutorialspoint.com/java/java_exceptions.htm)
- [ThoughtCo: Diferencia entre salida de error y excepciones en Java](https://www.thoughtco.com/java-programming-basics-2034180)