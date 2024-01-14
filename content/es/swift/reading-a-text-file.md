---
title:                "Swift: Leyendo un archivo de texto"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Por qué leer un archivo de texto?

Leer un archivo de texto es una habilidad esencial en el desarrollo de aplicaciones. Permite a los programadores trabajar con grandes cantidades de datos de manera más eficiente y procesar información estructurada en sus aplicaciones. Además, los archivos de texto son una forma universal de almacenar y compartir datos, lo que los convierte en una herramienta útil para colaborar con otros desarrolladores.

## Cómo hacerlo

La lectura de un archivo de texto en Swift es un proceso sencillo. Primero, debemos crear una instancia de la clase `FileManager` para manejar la interacción con nuestro sistema de archivos. Luego, utilizamos el método `contents(atPath:)` para obtener los datos del archivo y el método `String(data:encoding:)` para convertir esos datos en una cadena de texto legible. ¡Veamos un ejemplo!

```Swift
let fileManager = FileManager.default //Instanciamos FileManager

// Obtenemos el archivo de texto en la ruta específica
guard let fileData = fileManager.contents(atPath: "/Users/Usuario/Documentos/miArchivo.txt") else { 
    print("No se pudo obtener el archivo")
    return
}

// Convertimos los datos en una cadena de texto usando UTF-8 como codificación
let stringData = String(data: fileData, encoding: .utf8)
print(stringData)
```

Si nuestro archivo de texto tiene una estructura diferente, es posible que necesitemos utilizar el método `components(separatedBy:)` para dividirlo en partes más pequeñas y luego trabajar con esas partes por separado.

## Profundizando

Algunas veces, puede que necesitemos realizar operaciones más complejas con un archivo de texto. Por ejemplo, podemos querer leer solo una parte específica del archivo, como una línea o un párrafo en particular. Para esto, podemos utilizar el método `components(separatedBy:)` junto con el método `subscript()` de las cadenas de texto para acceder a la parte deseada.

También es importante mencionar que siempre debemos asegurarnos de manejar adecuadamente los posibles errores que puedan ocurrir al intentar leer un archivo de texto, como puede ser que el archivo no exista en la ruta especificada.

## Ver también

- [Documentación de FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Tutorial de Swift: leer y escribir archivos](https://www.hackingwithswift.com/read/contents/contentsfilemanager)