---
title:    "Swift: Redactar un archivo de texto"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Por qué escribir un archivo de texto en Swift

Es posible que te preguntes por qué deberías considerar escribir un archivo de texto en Swift. Bueno, hay varias razones por las que podría ser útil para ti. Tal vez necesitas almacenar datos de forma permanente, o quieres crear un informe o un archivo CSV para compartir con otros. Sea cual sea el motivo, escribir archivos de texto en Swift es una habilidad útil para tener en tu arsenal de programación.

## Cómo escribir un archivo de texto en Swift

Ahora que sabes por qué escribir archivos de texto en Swift puede ser beneficioso, veamos cómo hacerlo. En primer lugar, debes crear un objeto de tipo `FileManager` para acceder al sistema de archivos. Luego, puedes usar el método `createFile(atPath:contents:attributes:)` para crear el archivo en la ubicación deseada. A continuación, puedes escribir el contenido que deseas en el archivo usando el método `write` de `String`. Por último, asegúrate de cerrar el archivo después de haber terminado de escribir en él.

```
guard let fileManager = FileManager.default
    else { fatalError("Error al obtener el FileManager.") }
let filePath = "ruta/al/archivo.txt"
let fileContent = "Este es un archivo de texto escrito en Swift."
fileManager.createFile(atPath: filePath, contents: fileContent.data(using: .utf8), attributes: nil)
let fileHandle = FileHandle(forWritingAtPath: filePath)
fileHandle?.write(fileContent.data(using: .utf8)!)
fileHandle?.closeFile()
```

Si todo fue bien, deberías ver el archivo de texto en la ubicación especificada con el contenido que le has agregado.

## Profundizando en la escritura de archivos de texto en Swift

Ahora que ya sabes cómo escribir archivos de texto en Swift, es importante entender algunas cosas adicionales sobre este proceso. Por ejemplo, si deseas añadir más contenido a un archivo existente, puedes usar el método `append` en lugar de `write` en el manejador de archivo. También es importante tener en cuenta que puedes especificar atributos para el archivo, como su nombre, tipo y permisos.

Además, existen otras formas de escribir archivos de texto en Swift, como el uso de la biblioteca `Foundation` para trabajar con subprocesos y cadenas más grandes. Sin embargo, el método mencionado anteriormente es una forma rápida y sencilla de empezar a escribir archivos de texto en Swift.

## Ver También

- [Documentación oficial de Apple sobre FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Guía de Swift para escribir y leer archivos](https://www.hackingwithswift.com/read/12/overview)
- [Tutorial de Swift: Escribiendo y leyendo archivos de texto](https://www.tutorialspoint.com/swift/swift_writing_files.htm)