---
title:                "Escribiendo un archivo de texto"
html_title:           "Swift: Escribiendo un archivo de texto"
simple_title:         "Escribiendo un archivo de texto"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/writing-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Escribir un archivo de texto significa guardar información en un archivo que puede ser leído y editado por humanos. Los programadores suelen hacer esto para almacenar datos o configuraciones importantes que puedan ser modificados en el futuro.

## Cómo:

```Swift
// Creamos una ruta para nuestro archivo de texto
let filePath = "Users/miNombre/MisDocumentos/archivo.txt"

// Creamos una variable para almacenar el contenido del archivo
var contenido = "Este es un ejemplo de texto a guardar en un archivo."

// Intentamos escribir en el archivo
do {
    // Escribimos el contenido en el archivo usando la ruta y el método write
    try contenido.write(toFile: filePath, atomically: true, encoding: .utf8)
    print("¡Archivo de texto creado exitosamente!")
} catch {
    // En caso de error, imprimimos el mensaje correspondiente
    print("Hubo un error al crear el archivo de texto.")
}

// Si todo sale bien, podremos ver el archivo de texto en nuestra ruta especificada con el contenido guardado en él.
```

## Inmersión Profunda

Escribir archivos de texto es una técnica común en la programación y se ha utilizado desde los inicios de los sistemas informáticos. Existen también otros métodos para almacenar información, como bases de datos o archivos binarios, pero escribir un archivo de texto permite una fácil lectura y edición por parte de los programadores.

## Ver También

https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html