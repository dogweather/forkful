---
title:    "C#: Leyendo un archivo de texto"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué

Muchas veces, como programadores, necesitamos leer y procesar un archivo de texto en nuestros programas. Puede ser para obtener datos, configuraciones o simplemente para mostrar información al usuario. En este artículo, veremos cómo podemos leer un archivo de texto utilizando C#.

## Cómo hacerlo

Para leer un archivo de texto en C#, necesitamos utilizar la clase `StreamReader`. Esta clase nos permite leer caracteres de un stream de entrada, que en este caso será nuestro archivo de texto. En primer lugar, debemos asegurarnos de tener la librería `System.IO` incluida en nuestro programa.

Una vez que tengamos la librería, podemos utilizar el siguiente código para leer el archivo de texto:

```
using System.IO;

StreamReader sr = new StreamReader("miarchivo.txt");
string contenido = sr.ReadToEnd();
sr.Close();
```

En este caso, estamos creando una instancia de `StreamReader` y pasando como argumento el nombre del archivo que queremos leer. Luego, utilizamos el método `ReadToEnd()` para obtener el contenido completo del archivo en una variable de tipo string. Por último, debemos asegurarnos de cerrar el `StreamReader` para liberar los recursos utilizados.

## Profundizando

Además de `ReadToEnd()`, la clase `StreamReader` también cuenta con otros métodos útiles para leer un archivo de texto. Por ejemplo, podemos utilizar `ReadLine()` para leer una línea del archivo a la vez, o `Read()` para leer un caracter en particular. También podemos indicar el encoding del archivo que estamos leyendo, en caso de que sea necesario.

Es importante tener en cuenta que es posible que ocurran errores durante la lectura del archivo, por lo que es recomendable utilizar `try-catch` para manejar estas excepciones.

## Ver también

- [Documentación oficial de Microsoft sobre StreamReader](https://docs.microsoft.com/es-mx/dotnet/api/system.io.streamreader?view=net-5.0)
- [Ejemplo práctico de lectura de archivos de texto en C#](https://www.c-sharpcorner.com/UploadFile/mahesh/readfiletreamreader03092009094816AM/readfiletreamreader.aspx)

¡Espero que este artículo haya sido útil para aprender a leer archivos de texto en C#! No olvides practicar y probar diferentes métodos para encontrar el que mejor se adapte a tus necesidades. ¡Hasta la próxima!