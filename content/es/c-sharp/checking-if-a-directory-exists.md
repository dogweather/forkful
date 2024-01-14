---
title:    "C#: Comprobando si existe un directorio"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## ¿Por qué deberías verificar si un directorio existe?

Verificar si un directorio existe es una parte importante de la programación en C#. Esto te permite asegurarte de que un directorio específico está presente antes de intentar acceder a él o realizar operaciones en su interior. También ayuda a evitar errores inesperados en tu programa.

## Cómo verificar si un directorio existe en C#

Para verificar si un directorio existe en C#, usamos el método `Directory.Exists()` que devuelve `true` si el directorio existe y `false` si no existe. Aquí hay un ejemplo de cómo usar este método:

```C#
if (Directory.Exists(@ "C:\Users\Usuario\Documents"))
{
    Console.WriteLine("El directorio existe.");
}
else
{
    Console.WriteLine("El directorio no existe.");
}

// Resultado:
// El directorio existe.
```

También podemos combinar este método con la clase `Path` para verificar si un directorio existe en una ubicación específica. Por ejemplo:

```C#
string path = Path.Combine(@"C:\Users\Usuario", "Documents");

if (Directory.Exists(path))
{
    Console.WriteLine("El directorio existe.");
}
else
{
    Console.WriteLine("El directorio no existe.");
}

// Resultado:
// El directorio existe.
```

## En profundidad: Verificación de directorios en C#

Además de usar el método `Directory.Exists()`, también podemos usar otros métodos de la clase `Directory` para realizar operaciones en un directorio. Por ejemplo, podemos crear un directorio si no existe o listar los archivos y subdirectorios en un directorio específico.

También podemos utilizar la clase `FileInfo` para obtener información más detallada sobre un directorio existente, como su tamaño o su fecha de creación.

Es importante tener en cuenta que al trabajar con directorios, debemos tener permisos suficientes para acceder a ellos. De lo contrario, podemos recibir un error de acceso no autorizado.

## Ver también

- [Microsoft Docs: Directory.Exists()](https://docs.microsoft.com/es-es/dotnet/api/system.io.directory.exists?view=net-5.0)
- [Microsoft Docs: Crear un directorio (solo lectura) en C#](https://docs.microsoft.com/es-es/dotnet/standard/io/how-to-create-a-directory-only-if-it-doesnt-already-exist?view=net-5.0)
- [TechNet: Crear un directorio en C#](https://social.technet.microsoft.com/wiki/contents/articles/27668.crear-un-directorio-en-c.aspx)