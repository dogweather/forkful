---
title:                "C#: Comprobando si existe un directorio"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por qué

Al programar en C#, es importante verificar si un directorio existe antes de realizar operaciones en él. Esto ayuda a asegurar que el código se ejecute sin errores y que los archivos se guarden correctamente.

## Cómo hacerlo

Para verificar si un directorio existe en C#, primero necesitamos importar el espacio de nombres `System.IO`. Luego, podemos utilizar el método `Directory.Exists()` para comprobar si un directorio existe.

```C#
using System.IO;

Directory.Exists("ruta/del/directorio");
```

El método `Directory.Exists()` devolverá un valor booleano: `true` si el directorio existe y `false` si no existe.

## Profundizando

Cuando utilizamos el método `Directory.Exists()`, es importante tener en cuenta algunas cosas:

- Este método solo comprueba si el directorio existe, no si es un directorio válido o si tenemos permisos para acceder a él. Podríamos obtener `true` incluso si el directorio está dañado o es inaccesible.
- Podemos pasar una ruta absoluta o relativa como argumento. Si utilizamos una ruta relativa, se comprobará si el directorio existe en relación con el directorio actual.

Por ejemplo, si tenemos la siguiente estructura de directorios:

```
- MiProyecto
    - src
        - archivo.cs
    - datos
        - archivo.txt
```

Si ejecutamos `Directory.Exists("datos")` desde el archivo `archivo.cs`, obtendremos `true` ya que existe una carpeta llamada "datos" en la misma ubicación.

## Ver también

- [Documentación oficial de Microsoft sobre el método Directory.Exists()](https://docs.microsoft.com/es-es/dotnet/api/system.io.directory.exists?view=net-5.0)
- [Tutorial de C# sobre la gestión de archivos y directorios](https://www.c-sharpcorner.com/article/file-and-directory-management-in-C-Sharp/) 
- [Ejemplos de código para verificar si un directorio existe en C#](https://www.geeksforgeeks.org/check-if-a-folder-exists-in-c-sharp/)