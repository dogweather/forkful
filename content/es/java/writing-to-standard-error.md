---
title:                "Escribiendo en el error estándar"
html_title:           "Java: Escribiendo en el error estándar"
simple_title:         "Escribiendo en el error estándar"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Escribir en el error estándar es una técnica común en la programación en Java. Se utiliza para imprimir mensajes de error y diagnóstico mientras se ejecuta un programa. Los programadores lo hacen para ayudar a identificar y solucionar problemas en su código de manera eficiente.

## Cómo hacerlo:

Puedes escribir en el error estándar utilizando la clase System de Java y su método "err". Aquí hay un ejemplo de código:

```Java
System.err.println("¡Este es un mensaje de error!");
```

La salida de este código se verá así en tu consola:

```
¡Este es un mensaje de error!
```

## Profundizando:

Esta técnica se ha utilizado durante décadas en el desarrollo de software. Antes de la introducción de las herramientas de depuración modernas, los programadores dependían de imprimir mensajes en el error estándar para identificar y solucionar problemas en su código.

Aunque escribir en el error estándar sigue siendo una técnica útil, hay muchas otras formas de depurar código, como la depuración en tiempo real y las herramientas de seguimiento de pila. Sin embargo, aún es una herramienta importante en el proceso de resolución de problemas para muchos programadores.

Si bien el uso de System.err es una forma sencilla y directa de escribir en el error estándar, existen otras formas más avanzadas de realizar esta tarea utilizando las clases PrintStream y PrintWriter.

## Ver también:

- Documentación oficial de Java sobre System.err: https://docs.oracle.com/javase/8/docs/api/java/lang/System.html#err
- Artículo de StackOverflow sobre cómo escribir en el error estándar en Java: https://stackoverflow.com/questions/26459705/writing-to-standard-error