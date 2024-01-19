---
title:                "Imprimiendo salida de depuración"
html_title:           "Arduino: Imprimiendo salida de depuración"
simple_title:         "Imprimiendo salida de depuración"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/printing-debug-output.md"
---

{{< edit_this_page >}}

# "## ¿Qué y Por Qué?"

Imprimir la salida de depuración en Java te permite rastrear y entender el comportamiento de tu código mientras se está ejecutando. Lo hacemos principalmente para detectar y solucionar errores en nuestras aplicaciones.

# "## Cómo Hacerlo:"

Para imprimir la salida de depuración en Java, generalmente utilizamos `System.out.println()`. Aquí hay un ejemplo:

```Java
public class Main {
    public static void main(String[] args) {
        for(int i=0; i<5; i++) {
            System.out.println("El número es: " + i);
        }
    }
}
```

Este script imprimirá en la consola:

```
El número es: 0
El número es: 1
El número es: 2
El número es: 3
El número es: 4
```

# "## Buceo Profundo":

Históricamente hablando, imprimir salida de depuración ha sido una de las formas más sencillas de rastrear el comportamiento de nuestras aplicaciones desde los primeros días de la programación. Aunque existen alternativas más elegantes y robustas, como el uso de depuradores y trazadores de pila, la impresión de la salida de depuración sigue siendo una herramienta útil y directa en la arsenal del programador.

Una curiosidad sobre `System.out.println()` es que, en realidad, Java simplemente está accediendo a una variable estática "out" en la clase "System", que es una instancia de la clase "PrintStream". Este objeto "PrintStream" tiene un método "println()", que es el que utilizamos para imprimir en la consola.

Existen alternativas, por ejemplo, usar un logger como `java.util.logging` o un log4j. Estas librerías ofrecen gran flexibilidad y opciones de configuración avanzadas, permitiéndonos, por ejemplo, escribir la salida de depuración en un archivo en lugar de la consola.

```Java
import java.util.logging.Logger;

public class Main {
    private final static Logger LOGGER = Logger.getLogger(Main.class.getName());

    public static void main(String[] args) {
        for(int i=0; i<5; i++) {
            LOGGER.info("El número es: " + i);
        }
    }
}
```

# "## Ver También":

Encuentra más información en las siguientes fuentes:

1. [The Java Tutorials: Logging](https://docs.oracle.com/javase/tutorial/essential/logging/) (en ingles)
2. [Stack Overflow: System.out.println vs Logger](https://stackoverflow.com/questions/513600/should-i-use-java-util-logging-or-org-apache-log4j) (en ingles)
3. [Tutorial de Oracle: Java System Class](https://docs.oracle.com/javase/tutorial/essential/environment/system.html) (en ingles)