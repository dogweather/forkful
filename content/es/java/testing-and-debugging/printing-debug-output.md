---
title:                "Imprimiendo salida de depuración"
aliases: - /es/java/printing-debug-output.md
date:                  2024-01-20T17:52:50.266460-07:00
model:                 gpt-4-1106-preview
simple_title:         "Imprimiendo salida de depuración"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/printing-debug-output.md"
---

{{< edit_this_page >}}

## Qué & Por Qué?
Imprimir mensajes de depuración es el arte de mostrar información temporal en la consola para entender qué está pasando en nuestro código. Los programadores lo hacen para diagnosticar y corregir errores más fácilmente.

## Cómo hacerlo:
Aquí tienes un ejemplo sencillo:

```java
public class DebugExample {
    public static void main(String[] args) {
        int suma = 5 + 5;
        System.out.println("La suma es: " + suma); // Esto es un mensaje de depuración
    }
}
```

Output:
```
La suma es: 10
```

Para mensajes de depuración más avanzados, podemos usar `java.util.logging`:

```java
import java.util.logging.Logger;
import java.util.logging.Level;

public class AdvancedDebugExample {
    private static final Logger logger = Logger.getLogger(AdvancedDebugExample.class.getName());

    public static void main(String[] args) {
        try {
            int resultado = divide(10, 0);
            logger.log(Level.INFO, "El resultado es: " + resultado);
        } catch (ArithmeticException e) {
            logger.log(Level.SEVERE, "Error al dividir!", e);
        }
    }

    private static int divide(int a, int b) {
        return a / b;
    }
}
```

## Deep Dive:
Antes, imprimir en la consola con `System.out.println()` era suficiente. Pero conforme las aplicaciones crecieron, fue necesario desarrollar sistemas de logging más sofisticados. Ahora tenemos niveles de logging como INFO, WARNING y SEVERE en `java.util.logging`, y bibliotecas externas como Log4j que ofrecen más funcionalidades. Una práctica común es desactivar o redireccionar estos mensajes en producción para optimizar el rendimiento.

Java proporciona varias maneras de imprimir mensajes de depuración, desde la simple llamada a `System.out.println()` hasta sistemas de logging complejos con varias opciones de configuración. Elegir uno u otro dependerá del tamaño de la aplicación y de las necesidades específicas del proyecto.

## See Also:
- [java.util.logging documentation](https://docs.oracle.com/en/java/javase/17/docs/api/java.logging/java/util/logging/package-summary.html)
- [Apache Log4j 2](https://logging.apache.org/log4j/2.x/)
- [Baeldung on Java Logging](https://www.baeldung.com/java-logging-intro)
