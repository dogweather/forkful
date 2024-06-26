---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:31.994630-07:00
description: "C\xF3mo hacerlo: El framework Foundation de Swift proporciona la clase\
  \ `FileManager`, que tiene m\xE9todos para gestionar el sistema de archivos. Puedes\
  \ usar\u2026"
lastmod: '2024-03-13T22:44:59.430670-06:00'
model: gpt-4-0125-preview
summary: "El framework Foundation de Swift proporciona la clase `FileManager`, que\
  \ tiene m\xE9todos para gestionar el sistema de archivos."
title: Comprobando si un directorio existe
weight: 20
---

## Cómo hacerlo:
El framework Foundation de Swift proporciona la clase `FileManager`, que tiene métodos para gestionar el sistema de archivos. Puedes usar `FileManager` para comprobar si un directorio existe. Aquí tienes un fragmento sobre cómo hacer esto:

```swift
import Foundation

let fileManager = FileManager.default
let path = "/ruta/a/tu/directorio"

if fileManager.fileExists(atPath: path, isDirectory: nil) {
    print("El directorio existe")
} else {
    print("El directorio no existe")
}
```

Sin embargo, esto comprueba tanto archivos como directorios. Si específicamente quieres verificar si un directorio existe, necesitas pasar un puntero a un valor booleano en `isDirectory`:

```swift
import Foundation

let fileManager = FileManager.default
let path = "/ruta/a/tu/directorio"
var isDirectory: ObjCBool = false

if fileManager.fileExists(atPath: path, isDirectory: &isDirectory), isDirectory.boolValue {
    print("El directorio existe")
} else {
    print("El directorio no existe")
}
```

### Usando una Biblioteca de Terceros
Hasta ahora, comprobar la existencia de un directorio en Swift generalmente no requiere bibliotecas de terceros debido a la robustez de la clase `FileManager`. Sin embargo, para manipulaciones y comprobaciones de archivos más complejas, bibliotecas como **Files** de John Sundell proporcionan una API más amigable para Swift.

Así es como podrías usarlo:

Primero, añade Files a tu proyecto a través de Swift Package Manager.

Luego, puedes comprobar la existencia de un directorio así:

```swift
import Files

do {
    _ = try Folder(path: "/ruta/a/tu/directorio")
    print("El directorio existe")
} catch {
    print("El directorio no existe")
}
```

Nota: Como las bibliotecas de terceros pueden cambiar, siempre consulta la documentación más reciente para el uso y las mejores prácticas.
