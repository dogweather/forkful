---
title:                "Java: Leyendo argumentos de línea de comandos"
simple_title:         "Leyendo argumentos de línea de comandos"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por qué
¿Alguna vez te has preguntado cómo puedes interactuar con un programa a través de la línea de comandos? ¡La respuesta es leyendo argumentos de línea de comandos! En este artículo, te mostraremos cómo puedes leer y utilizar los argumentos ingresados por el usuario en la línea de comandos usando Java.

## Cómo hacerlo
Primero, necesitamos entender qué son los argumentos de línea de comandos. Son valores que se ingresan por el usuario en la línea de comandos después de llamar al programa. Por ejemplo, si nuestro programa se llama "contador" y queremos contar hasta un número específico, podríamos llamar al programa de la siguiente manera: `java contador 10`. El número 10 sería considerado un argumento de línea de comandos.

Ahora, veamos un ejemplo de cómo podemos leer estos argumentos en nuestro programa usando Java:

```Java
public class Contador {

  public static void main(String[] args) {

    //Verificamos si hay un argumento ingresado
    if (args.length > 0) {
      //Convertimos el argumento a un número entero
      int limite = Integer.parseInt(args[0]);
      //Imprimimos una cuenta regresiva hasta el límite ingresado
      for (int i = limite; i > 0; i--) {
        System.out.println(i);
      }
    } else {
      //Si no hay argumento, imprimimos un mensaje de error
      System.out.println("Por favor ingrese un número como argumento.");
    }
  }
}
```

Si ejecutamos este programa con el argumento "10", obtendremos la siguiente salida:

```
10
9
8
7
6
5
4
3
2
1
```

## Profundizando
Ahora que sabemos cómo leer los argumentos de línea de comandos en Java, podemos ir un poco más allá y entender cómo se almacenan estos argumentos en el arreglo `args`.

Cuando ejecutamos nuestro programa, cada argumento ingresado se guarda como un elemento en `args`, y podemos acceder a ellos usando su índice. Por ejemplo, si ingresamos los argumentos `java contador 10 20`, el valor "10" se almacenaría en `args[0]`, y el valor "20" en `args[1]`.

También es importante recordar que los argumentos se almacenan como cadenas de texto en `args`, por lo que es necesario realizar conversiones si queremos utilizarlos como otros tipos de datos (como en nuestro ejemplo anterior donde convertimos el argumento a un número entero).

## Ver también
- [Documentación oficial de Java sobre argumentos de línea de comandos](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
- [Tutorial de Oracle sobre la clase `java.lang.Integer`](https://docs.oracle.com/javase/8/docs/api/java/lang/Integer.html)
- [Ejemplos de código para leer argumentos de línea de comandos en Java](https://www.javatpoint.com/Command-line-arguments-in-java)