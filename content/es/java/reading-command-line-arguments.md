---
title:                "Leyendo argumentos de la línea de comandos"
html_title:           "Bash: Leyendo argumentos de la línea de comandos"
simple_title:         "Leyendo argumentos de la línea de comandos"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

---
## ¿Qué y Por Qué?

Leer argumentos de la línea de comandos significa obtener datos de entrada directamente desde tu terminal o consola. Programadores lo usan para personalizar cómo funciona su programa dependiendo de las entradas que el usuario proporciona.

## ¿Cómo se hace?

Considere este sencillo ejemplo:

```Java
public class Main {
    public static void main(String[] args) {
        for (String arg: args) {
            System.out.println(arg);
        }
    }
}
```

Para ejecutar este programa con argumentos de línea de comandos, escriba algo similar en su terminal:

```shell
java Main Hola Mundo
```

El output será:

```
Hola
Mundo
```

## Profundizamos

Los argumentos de la línea de comandos no son nada nuevo. Han existido desde los primeros días del desarrollo de software. Sin embargo, aunque pueda parecer un concepto simple, surgen complejidades durante la implementación, como el manejo de tipos de datos, las referencias a archivos o la gestión de múltiples argumentos.

Existen alternativas para leer inputs, como las entradas estándar y los archivos de configuración. Sin embargo, para una entrada rápida y eficiente, los argumentos de la línea de comandos suelen ser suficientes.

El array `args` en el `main` funciona como el receptor de los argumentos de la línea de comandos en Java. Cada elemento de la matriz `args` es una `String`, aunque usted introduzca un número como argumento. Si necesita utilizar el argumento como un tipo de dato distinto, tendrá que convertirlo apropiadamente.

## Ver También

[Documentación Oficial de Oracle](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)

[Cómo usar Argumentos de Línea de Comandos en Java](https://www.baeldung.com/java-command-line-arguments)