---
title:    "Swift: Escribiendo un archivo de texto"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/swift/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué escribir un archivo de texto

Escribir archivos de texto es una tarea común en el mundo de la programación, ya que permite almacenar información de manera sencilla y estructurada. Además, es una habilidad fundamental para cualquier desarrollador que quiera guardar y compartir datos en sus aplicaciones.

## Cómo hacerlo

Para escribir un archivo de texto en Swift, podemos utilizar la clase `FileManager` y su método `createFile`. A continuación, se muestra un ejemplo de cómo crear un archivo de texto llamado "miArchivo.txt" y escribir en él la frase "¡Hola, mundo!":

```
Swift
let texto = "¡Hola, mundo!"
let url = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask)[0].appendingPathComponent("miArchivo.txt")
do {
    try texto.write(to: url, atomically: true, encoding: .utf8)
} catch {
    print(error)
}
```

El método `urls` nos permite obtener la ruta del directorio de documentos de nuestra aplicación, mientras que `appendingPathComponent` nos permite agregarle un nombre y extensión al archivo. Luego, utilizamos `write` para escribir el texto en la ruta especificada.

## Profundizando en la escritura de archivos de texto

Además de escribir en un archivo, también podemos agregar más contenido a un archivo ya existente utilizando el método `append`. También es posible leer y modificar archivos utilizando diferentes métodos de la clase `FileManager`. Es importante tener en cuenta que, al escribir en un archivo, siempre debemos manejar correctamente posibles errores utilizando `try-catch` o `do-catch`.

## Ver también

Aquí tienes algunos enlaces que pueden ser útiles para aprender más sobre cómo escribir archivos de texto en Swift:

- [Documentación oficial de Apple sobre FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Tutorial de Ray Wenderlich sobre trabajo con archivos en Swift](https://www.raywenderlich.com/4974-saving-data-in-ios-12-with-core-data?utm_source=raywenderlich.com+Weekly&utm_campaign=8d1845ad6a-raywenderlich_com_Weekly_Issue_165&utm_medium=email&utm_term=0_83b6edc87f-8d1845ad6a-415822821)
- [Artículo de Hacking with Swift sobre escritura de archivos](https://www.hackingwithswift.com/example-code/system/how-to-read-and-write-strings-in-text-files)