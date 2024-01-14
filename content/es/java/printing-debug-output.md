---
title:                "Java: Imprimiendo la salida de depuración"
programming_language: "Java"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por qué

Imprimir mensajes de depuración en Java es una técnica muy útil para identificar errores y entender cómo funciona nuestro código. Con la impresión de mensajes de depuración, podemos ver el valor de las variables en diferentes puntos de nuestro programa y encontrar posibles errores en la lógica del mismo.

## ¿Cómo hacerlo?

Para imprimir mensajes de depuración en Java, utilizamos el método `System.out.println()`. Este método toma como argumento una cadena de texto o el valor de una variable y lo imprime en la consola. Veamos un ejemplo:

```Java
int a = 5;
System.out.println("El valor de a es: " + a);
```

El resultado de este código sería `El valor de a es: 5`, ya que la variable `a` tiene asignado el valor de 5. También podemos imprimir el valor de una variable dentro de una cadena de texto utilizando la notación de `%s` para cadenas y `%d` para números:

```Java
String nombre = "Juan";
int edad = 25;
System.out.printf("Mi nombre es %s y tengo %d años.", nombre, edad);
```

El resultado de este ejemplo sería `Mi nombre es Juan y tengo 25 años.`.

## Profundizando

Las impresiones de debug no solo nos sirven para ver el valor de las variables, sino también para entender cómo fluye nuestro código. Podemos agregar diferentes mensajes en puntos clave de nuestro código para verificar si se están cumpliendo ciertas condiciones. También podemos utilizar la palabra clave `if` para imprimir un mensaje solo si se cumple una determinada condición. Por ejemplo:

```Java
int b = 10;

if (b % 2 == 0) {
    System.out.println("b es un número par.");
} else {
    System.out.println("b es un número impar.");
}
```

El resultado de este código sería `b es un número par.` ya que el número 10 cumple con la condición de ser divisible entre 2. De esta forma, podemos verificar si nuestro código está funcionando de la manera deseada y encontrar posibles errores.

## Ver también

Para obtener más información sobre cómo imprimir mensajes de depuración en Java, puedes consultar la documentación oficial en [Java Debugging](https://docs.oracle.com/javase/8/docs/technotes/guides/jpda/index.html). También puedes aprender más sobre cómo utilizar el método `printf()` en la [documentación de Java](https://docs.oracle.com/javase/tutorial/java/data/manipstrings.html). ¡Prueba a utilizar impresiones de debug en tu próximo proyecto para ahorrar tiempo y esfuerzo en la identificación de errores!