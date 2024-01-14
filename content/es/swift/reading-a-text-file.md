---
title:    "Swift: Leyendo un archivo de texto"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Por qué

Muchas veces, al programar en Swift, necesitamos leer información desde un archivo de texto. Esto puede ser útil en situaciones como guardar datos de un juego o cargar configuraciones de una aplicación. Aprender cómo leer un archivo de texto nos permite tener un mayor control y flexibilidad en nuestro código.

## Cómo hacerlo

Para leer un archivo de texto en Swift, primero debemos obtener la ruta del archivo en nuestro dispositivo. En este ejemplo, supongamos que tenemos un archivo llamado "datos.txt" en nuestro escritorio.

```Swift
if let ruta = FileManager.default.urls(for: .desktopDirectory, in: .userDomainMask).first {
    let archivoURL = ruta.appendingPathComponent("datos.txt")
}
```

Una vez que tenemos la ruta del archivo, podemos leer su contenido usando el método `String(contentsOfURL:)` y asignarlo a una variable.

```Swift
if let ruta = FileManager.default.urls(for: .desktopDirectory, in: .userDomainMask).first {
    let archivoURL = ruta.appendingPathComponent("datos.txt")
    if let contenido = try? String(contentsOf: archivoURL, encoding: .utf8) {
        print(contenido)
    }
}
```

El resultado en consola será el contenido completo del archivo de texto.

## Profundizando

Además de simplemente leer el contenido de un archivo de texto, también podemos realizar operaciones más complejas, como por ejemplo leer línea por línea.

```Swift
if let ruta = FileManager.default.urls(for: .desktopDirectory, in: .userDomainMask).first {
    let archivoURL = ruta.appendingPathComponent("datos.txt")
    if let contenido = try? String(contentsOf: archivoURL, encoding: .utf8) {
        let lineas = contenido.components(separatedBy: .newlines)
        for linea in lineas {
            print(linea)
        }
    }
}
```

Esto nos permitirá trabajar con cada línea de manera individual, lo que puede ser útil en diferentes situaciones.

## Ver también

Para obtener más información sobre cómo leer archivos de texto en Swift, puedes revisar estos recursos:

- [Documentación oficial de Swift sobre manejo de archivos](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Tutorial sobre cómo leer y escribir archivos en Swift](https://www.hackingwithswift.com/read-and-write-files-with-uikit)