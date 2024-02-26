---
date: 2024-01-26 00:57:46.441814-07:00
description: "Manejar errores en Swift significa anticipar y responder a problemas\
  \ que surgen cuando tu c\xF3digo se ejecuta. Lo hacemos para controlar el caos\u2014\
  manteniendo\u2026"
lastmod: '2024-02-25T18:49:55.893333-07:00'
model: gpt-4-1106-preview
summary: "Manejar errores en Swift significa anticipar y responder a problemas que\
  \ surgen cuando tu c\xF3digo se ejecuta. Lo hacemos para controlar el caos\u2014\
  manteniendo\u2026"
title: Manejo de errores
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Manejar errores en Swift significa anticipar y responder a problemas que surgen cuando tu código se ejecuta. Lo hacemos para controlar el caos—manteniendo las aplicaciones sin caídas y ofreciendo al usuario una experiencia fluida.

## Cómo hacerlo:
Swift utiliza el manejo de errores con los bloques `do`, `try` y `catch`. Echemos un vistazo:

```Swift
enum FileError: Error {
    case fileDoesNotExist
    case noPermission
}

func readFile(atPath path: String) throws -> String {
    // Supongamos que tenemos aquí alguna lógica para comprobar si un archivo existe y si tenemos permiso para leerlo
    let fileExists = false
    let havePermission = true

    if !fileExists {
        throw FileError.fileDoesNotExist
    }

    if !havePermission {
        throw FileError.noPermission
    }

    return "El contenido del archivo va aquí"
}

do {
    let fileContent = try readFile(atPath: "/ruta/al/archivo")
    print(fileContent)
} catch FileError.fileDoesNotExist {
    print("¡Vaya! Archivo no encontrado.")
} catch FileError.noPermission {
    print("¡Ah! Sin permiso para leer el archivo.")
} catch {
    print("Ocurrió un error desconocido.")
}

```

Salida de muestra:

```
¡Vaya! Archivo no encontrado.
```

## Profundizando
El manejo de errores no siempre fue tan fluido como lo es ahora. En Objective-C, lidiabas con punteros a objetos NSError, lo que parecía torpe. Ahora, tenemos un sistema más elegante con enumeraciones de Swift y el protocolo `Error`.

El `throw` de Swift nos permite señalar que algo salió mal. Los bloques `do` actúan como dominios conscientes de errores, `try` llama a los negocios arriesgados, y `catch` maneja las cosas si se van al sur.

Los opcionales son una alternativa para situaciones que no tienen un estatus de "error", pero que aún podrían no tener "resultado". Son un poco como las variables de Schrödinger: tienen un valor o no lo tienen.

Para una profundización real, consulta los tipos `Result`, que son híbridos elegantes entre patrones de retorno regular y de error.

## También te puede interesar
- Guía Oficial de Manejo de Errores en Swift: [Documentos de Apple](https://docs.swift.org/swift-book/LanguageGuide/ErrorHandling.html)
- Mejores prácticas para el manejo de errores en Swift: [RayWenderlich.com](https://www.raywenderlich.com/1851-beginning-swift-error-handling)
- Manejo avanzado de errores en Swift: [Artículo en Medium](https://medium.com/better-programming/advanced-error-handling-in-swift-4f6bdf6b01d8)
