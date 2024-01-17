---
title:                "Escribiendo un archivo de texto"
html_title:           "C#: Escribiendo un archivo de texto"
simple_title:         "Escribiendo un archivo de texto"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Escribir un archivo de texto en C# es simplemente guardar información en un archivo de texto plano. Los programadores lo hacen principalmente para almacenar datos de forma estructurada y accesible para su uso en futuras operaciones.

## Cómo hacerlo:
Para escribir un archivo de texto en C#, primero necesitamos crear una instancia de la clase `StreamWriter` y especificar el nombre del archivo y la ruta donde queremos guardarlo. Luego, podemos utilizar el método `WriteLine()` para escribir diferentes líneas de información en el archivo. Finalmente, debemos cerrar el archivo utilizando el método `Close()` para asegurarnos de que todos los datos se guarden correctamente. Por ejemplo:

```C#
using (StreamWriter archivo = new StreamWriter("ruta/del/archivo.txt")) 
{
    archivo.WriteLine("Hola mundo!");
    archivo.WriteLine("Este es un ejemplo de cómo escribir un archivo de texto en C#.");
} 
```

Este código creará un archivo llamado "archivo.txt" en la ruta especificada y guardará las líneas "Hola mundo!" y "Este es un ejemplo de cómo escribir un archivo de texto en C#".

## Profundizando:
Antes de la invención de la tecnología de almacenamiento digital, los programadores solían escribir en cintas perforadas para guardar datos estructurados. Con el avance de la tecnología, se desarrollaron diferentes formas de almacenar datos, como los archivos de texto. En C#, también podemos utilizar la clase `File` para escribir un archivo de texto, pero la clase `StreamWriter` nos permite un mayor control sobre el formato de los datos.

## Ver también:
- Documentación oficial de Microsoft sobre la clase `StreamWriter` en C#: https://docs.microsoft.com/en-us/dotnet/api/system.io.streamwriter?view=netcore-3.1 
- Tutorial de C# para principiantes que cubre cómo escribir y leer archivos de texto: https://www.completecsharp.co.uk/writing-textfiles/