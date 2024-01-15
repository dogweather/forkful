---
title:                "Leyendo un archivo de texto"
html_title:           "Java: Leyendo un archivo de texto"
simple_title:         "Leyendo un archivo de texto"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué
Leer archivos de texto en Java es una habilidad fundamental para cualquier programador. Al poder leer y procesar este tipo de archivos, puedes automatizar tareas y manejar grandes cantidades de datos de forma eficiente.

## Cómo hacerlo
Para leer un archivo de texto en Java, primero debes crear un objeto de la clase `File` que represente el archivo que deseas leer.

```
File archivo = new File("ruta/del/archivo.txt");
```

Luego, debes crear un objeto de la clase `Scanner` que nos permitirá leer el contenido del archivo. Este objeto necesita como parámetro el objeto `File` que creamos antes.

```
Scanner lector = new Scanner(archivo);
```

Ahora, podemos utilizar el método `nextLine()` del objeto `Scanner` para leer cada línea del archivo.

```
while(lector.hasNextLine()){
    String linea = lector.nextLine();
    // Haz algo con cada línea
    System.out.println(linea);
}
```

Finalmente, es importante cerrar el objeto `Scanner` una vez que ya no lo necesitamos.

```
lector.close();
```

## Profundizando
Al leer un archivo de texto en Java, hay algunas consideraciones adicionales que debes tener en cuenta. Por ejemplo, puedes especificar el conjunto de caracteres que deseas utilizar al leer el archivo, lo cual puede ser útil si el archivo contiene caracteres especiales.

Para ello, puedes utilizar el constructor de la clase `Scanner` que recibe como parámetro un objeto `Charset`.

```
Scanner lector = new Scanner(archivo, StandardCharsets.UTF_8);
```

Además, puedes realizar diversas operaciones con la información leída, como por ejemplo, buscar y extraer ciertos patrones de texto utilizando expresiones regulares.

## Ver también
- [Documentación oficial de Java para la clase `File`](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/io/File.html)
- [Documentación oficial de Java para la clase `Scanner`](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/util/Scanner.html)
- [Tutorial de Java: Leyendo y escribiendo archivos de texto](https://docs.oracle.com/javase/tutorial/essential/io/file.html)