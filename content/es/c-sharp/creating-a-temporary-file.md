---
title:                "Creando un archivo temporal."
html_title:           "C#: Creando un archivo temporal."
simple_title:         "Creando un archivo temporal."
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por qué

Crear un archivo temporal es una práctica común en la programación en C# ya que puede ayudarnos a almacenar información de forma temporal y eliminarla una vez que ya no la necesitamos. Además, puede ser útil en situaciones en las que no queremos afectar a un archivo existente mientras realizamos cambios en él.

## Cómo hacerlo

Para crear un archivo temporal en C#, primero debemos importar el espacio de nombres `System.IO`. Luego, utilizaremos el método `Path.GetTempFileName()` para generar un nombre único para nuestro archivo temporal y guardarlo en una variable.

```
using System.IO;

string tempFilePath = Path.GetTempFileName();
```

Después, podemos escribir cualquier información que queramos almacenar en el archivo utilizando la clase `StreamWriter`, que nos permite escribir en un archivo de texto.

```
using (StreamWriter writer = new StreamWriter(tempFilePath))
{
    writer.WriteLine("Este es un archivo temporal creado en C#.");
    writer.WriteLine("Puedes agregar cualquier información que desees.");
}
```

Finalmente, para eliminar el archivo temporal después de su uso, podemos utilizar el método `File.Delete()` y pasarle la ruta del archivo como argumento.

```
File.Delete(tempFilePath);
```

## Profundizando

Si queremos tener un mayor control sobre la creación de un archivo temporal, podemos utilizar la clase `FileStream` en lugar de `StreamWriter`. Esto nos permite especificar el nombre y la ubicación del archivo temporal, así como su tamaño máximo y el modo de abrirlo.

Además, si el archivo temporal contiene información confidencial, podemos garantizar su eliminación utilizando un bloque `try-finally` y el método `File.Delete()` en el bloque `finally`. De esta manera, nos aseguramos de que el archivo sea eliminado incluso si se produce una excepción en el código.

## Ver también

- Documentación oficial de Microsoft sobre el método `GetTempFileName()`: https://docs.microsoft.com/es-es/dotnet/api/system.io.path.gettempfilename?view=net-5.0

- Ejemplos de uso de archivos temporales en C#: https://www.c-sharpcorner.com/UploadFile/mahesh/HOWTOCreateTemporaryFile11222005012400AM/HOWTOCreateTemporaryFile.aspx