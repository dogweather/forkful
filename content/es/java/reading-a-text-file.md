---
title:                "Java: Leyendo un archivo de texto"
simple_title:         "Leyendo un archivo de texto"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué

Leer un archivo de texto en un programa Java puede ser una tarea simple, pero es una habilidad esencial para cualquier programador. Puede ser útil para leer datos de entrada, como un archivo de configuración o un registro de errores, o para escribir datos de salida. Como programador, es importante tener el conocimiento y la comprensión necesarios para leer y manipular archivos de texto en Java.

## Cómo hacerlo

Para leer un archivo de texto en Java, primero necesitamos una instancia de la clase `File`, que representa el archivo en sí. Podemos crear un objeto `File` proporcionando la ruta al archivo como argumento del constructor. A continuación, utilizamos una instancia de la clase `BufferedReader`, que proporciona un método conveniente para leer líneas de texto de un archivo. Dentro de un bloque `try-catch`, podemos usar el método `readLine()` para leer cada línea del archivo en una variable `String` y luego imprimirla en la consola.

```Java
try {
    File file = new File("miarchivo.txt");
    BufferedReader reader = new BufferedReader(new FileReader(file));

    String line;
    while ((line = reader.readLine()) != null) {
        System.out.println(line);
    }

    reader.close();
} catch (IOException e) {
    System.out.println("No se pudo leer el archivo: " + e.getMessage());
}
```

Si el archivo que estamos leyendo no existe, se lanzará una excepción `IOException` y se imprimirá un mensaje de error en la consola. Por eso es importante envolver el código en un bloque `try-catch`.

## Profundizando

Ahora que sabemos cómo leer un archivo de texto en Java, podemos profundizar en algunas técnicas adicionales para manipular los datos que estamos leyendo. Por ejemplo, podemos usar el método `split()` para separar una línea de texto en diferentes partes basadas en un separador determinado. También podemos utilizar `BufferedReader` para leer caracteres individuales en lugar de líneas completas.

Además, Java también proporciona la clase `Scanner`, que permite leer archivos de texto de forma más sencilla y flexible. Es importante leer bien la documentación y comprender todas las opciones disponibles para procesar los datos del archivo de la manera más efectiva.

## Ver también

- [Documentación oficial para la clase `File`](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [Documentación oficial para la clase `BufferedReader`](https://docs.oracle.com/javase/8/docs/api/java/io/BufferedReader.html)
- [Documentación oficial para la clase `Scanner`](https://docs.oracle.com/javase/8/docs/api/java/util/Scanner.html)