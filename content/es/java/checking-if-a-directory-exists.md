---
title:    "Java: Comprobando si un directorio existe"
keywords: ["Java"]
---

{{< edit_this_page >}}

## ¿Por qué comprobar si un directorio existe?

Cuando escribimos un programa en Java, es importante tener en cuenta todas las posibles situaciones que pueden ocurrir al ejecutarlo. Una de estas situaciones es cuando nuestro programa intenta acceder a un directorio específico en el sistema de archivos. En este caso, es importante verificar si el directorio existe antes de intentar acceder a él.

## Cómo hacerlo

Para comprobar si un directorio existe en Java, podemos utilizar el método `exists()` de la clase `File`. Este método devuelve un valor booleano, `true` si el directorio existe y `false` si no. Veamos un ejemplo de código:

```Java
import java.io.File;

public class DirectorioExistente {

    public static void main(String[] args) {
        String nombreDirectorio = "mi_directorio";

        // Crear un objeto File con el nombre del directorio
        File directorio = new File(nombreDirectorio);

        // Comprobar si el directorio existe
        boolean existeDirectorio = directorio.exists();

        // Mostrar por pantalla el resultado
        System.out.println("¿El directorio " + nombreDirectorio + " existe? " + existeDirectorio);
    }
}
```

En este ejemplo, creamos un objeto `File` con el nombre del directorio que queremos comprobar. Luego utilizamos el método `exists()` para verificar si existe o no, y finalmente mostramos el resultado por pantalla.

Si el directorio existe, el programa imprimirá "¿El directorio mi_directorio existe? true". Si no existe, imprimirá "¿El directorio mi_directorio existe? false".

## Profundizando

El método `exists()` simplemente comprueba si el directorio existe en el sistema de archivos, pero no nos dice si es un directorio válido o no. Además, debemos tener en cuenta que el directorio puede ser eliminado en cualquier momento por otros procesos o por el usuario. Por lo tanto, es importante tomar medidas adicionales para garantizar que nuestro programa tenga un comportamiento adecuado en caso de que el directorio no exista.

Una forma de hacerlo es capturar la excepción `SecurityException` que puede ocurrir si nuestro programa no tiene permisos suficientes para acceder al directorio. También podemos utilizar el método `isDirectory()` para verificar si el objeto `File` es un directorio válido.

## Véase también

- [Java File class](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/io/File.html)
- [Tutorial: Java if/else statements](https://www.w3schools.com/java/java_conditions.asp)
- [Using Java to check if a directory exists](https://www.baeldung.com/java-check-if-directory-exists)