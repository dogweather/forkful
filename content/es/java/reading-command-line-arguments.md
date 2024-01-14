---
title:    "Java: Leyendo argumentos de línea de comando."
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por qué 

¿Has oído hablar alguna vez de los argumentos de línea de comando en Java? Si eres un programador principiante o alguien que se está familiarizando con Java, es posible que te hayas preguntado por qué es importante aprender a leer argumentos de línea de comando en Java. En este artículo, te explicaremos brevemente por qué es una habilidad útil para cualquier programador de Java y cómo puedes comenzar a hacerlo.

## Cómo hacerlo 

La lectura de argumentos de línea de comando en Java te permite pasar información al programa cuando lo estás ejecutando en la línea de comando. Esta habilidad te permite personalizar la ejecución de tu programa en función de la entrada ingresada por el usuario. A continuación, te mostramos un ejemplo de cómo hacerlo en Java:

```Java
public class MiPrograma {
    public static void main(String[] args) {
        System.out.println("Hola " + args[0] + "!");
    }
}
```

En el ejemplo anterior, estamos utilizando el argumento de línea de comando `args[0]` para imprimir un mensaje de saludo personalizado. Al ejecutar este programa desde la línea de comando, podemos pasar un argumento después del nombre del programa, por ejemplo `java MiPrograma Juan`, y el programa imprimirá `Hola Juan!`. Nota que `args[0]` siempre se refiere al primer argumento ingresado después del nombre del programa.

## Profundizando 

Ahora que ya sabes cómo leer argumentos de línea de comando en Java, es importante entender algunos detalles adicionales. Por ejemplo, ¿qué pasa si queremos pasar más de un argumento o queremos que el argumento se convierta a un tipo de dato distinto? Para ello, podemos utilizar la clase [Scanner](https://docs.oracle.com/javase/8/docs/api/java/util/Scanner.html) de Java que nos permite leer la entrada en diferentes formatos y convertirla a otros tipos de datos. A continuación, un ejemplo de cómo utilizar esta clase en nuestro programa:

```Java
import java.util.Scanner;

public class MiPrograma {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        System.out.println("¿Cuál es tu nombre? ");
        String nombre = scanner.nextLine();
        System.out.println("¿Y cuántos años tienes? ");
        int edad = scanner.nextInt();
        System.out.println("Hola " + nombre + "! Tienes " + edad + " años.");
    }
}
```

En este ejemplo, estamos utilizando `scanner.nextLine()` para leer una cadena de texto y `scanner.nextInt()` para leer un número entero. De esta manera, podemos personalizar aún más la interacción con nuestro programa a través de los argumentos de línea de comando y la entrada del usuario. Es importante tener en cuenta que la clase Scanner necesita ser importada en nuestro programa para poder ser utilizada.

## Ver también 

- [Documentación oficial de Java sobre argumentos de línea de comando](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
- [Tutorial de Baeldung sobre lectura de argumentos de línea de comando en Java](https://www.baeldung.com/java-command-line-arguments)
- [Artículo de Oracle sobre el uso de la clase Scanner en Java](https://education.oracle.com/java-class-scanner)

¡Esperamos que este artículo te haya sido útil para entender la importancia y cómo leer argumentos de línea de comando en Java! ¡No dudes en compartir tus comentarios o preguntas en la sección de comentarios a continuación. ¡Hasta la próxima!