---
title:                "C#: Leyendo un archivo de texto"
simple_title:         "Leyendo un archivo de texto"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué

Si eres un programador en C#, seguramente te hayas encontrado en la situación de tener que leer un archivo de texto en tu código. Ya sea para obtener información de un usuario, importar datos o realizar algún tipo de cálculo, es importante saber cómo leer un archivo de texto de manera eficiente. En este artículo, te mostraremos cómo hacerlo en C# de una manera sencilla y efectiva.

## Cómo hacerlo

Para leer un archivo de texto en C#, vamos a utilizar la clase `StreamReader` de la librería `System.IO`. Esta clase nos permite leer caracteres desde un flujo de datos en C#. Primero, debemos crear una instancia de la clase `StreamReader` pasándole como argumento la ruta del archivo que queremos leer. Por ejemplo:

```C#
StreamReader sr = new StreamReader("archivo.txt");
```

Luego, podemos utilizar el método `ReadLine()` de la clase  `StreamReader` para leer cada línea del archivo de texto en una variable de tipo `string`. El método `ReadLine()` devuelve la siguiente línea de caracteres del archivo y se mueve al siguiente salto de línea en cada llamada. Por ejemplo:

```C#
string linea = sr.ReadLine();
```

Si queremos leer el archivo completo, podemos utilizar un bucle `while` y una variable auxiliar para almacenar cada línea leída y mostrarla en pantalla. Por ejemplo:

```C#
string linea;
while ((linea = sr.ReadLine()) != null)
{
    Console.WriteLine(linea);
}
```

Finalmente, para asegurarse de que se liberen todos los recursos utilizados, es importante cerrar el `StreamReader` utilizando el método `Close()`. Así quedará nuestro código completo:

```C#
StreamReader sr = new StreamReader("archivo.txt");
string linea;
while ((linea = sr.ReadLine()) != null)
{
    Console.WriteLine(linea);
}
sr.Close();
```

Como puedes ver, es fácil leer un archivo de texto en C# utilizando la clase `StreamReader`. Puedes adaptar este código a tus necesidades para leer el archivo de texto de manera eficiente.

## Profundizando

Si deseas obtener más información sobre cómo leer un archivo de texto en C#, puedes echar un vistazo a la documentación oficial de Microsoft sobre la clase `StreamReader` y sus métodos. También puedes probar diferentes maneras de leer archivos de texto, como utilizando la clase `File` de la librería `System.IO` o el método `ReadAllLines()`. ¡Experimenta y encuentra la forma que mejor se adapte a tu proyecto!

## Ver también

- [Documentación oficial de Microsoft sobre la clase `StreamReader`](https://docs.microsoft.com/es-es/dotnet/api/system.io.streamreader)
- [Ejemplo de lectura de archivos de texto en C#](https://www.c-sharpcorner.com/UploadFile/a20beb/file-read-and-display-content-in-net/)
- [Consejos para leer y escribir archivos de texto en C#](https://www.codeproject.com/Articles/16019/C-File-Read-Write-Using-StreamReader-and-StreamWri)