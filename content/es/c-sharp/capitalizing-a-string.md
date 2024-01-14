---
title:                "C#: Capitalizar una cadena"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por qué capitalizar una cadena en C#

Capitalizar una cadena en C# es una tarea común en la programación ya que permite mejorar la legibilidad de los textos para los usuarios. Al capitalizar una cadena, se pueden resaltar términos importantes o hacer que un texto se vea más profesional y organizado. Además, puede ser necesario en ciertas situaciones para cumplir con estándares de formato de texto.

## Cómo capitalizar una cadena en C#

La función de capitalización en C# se conoce como "ToUpper()" y se puede usar en una cadena para convertir todos los caracteres a mayúsculas. Por ejemplo, si tenemos una cadena llamada "hola mundo", al aplicar la función "ToUpper()", obtendremos "HOLA MUNDO".

```C#
string cadena = "hola mundo";
string capitalizada = cadena.ToUpper();
Console.WriteLine(capitalizada);
```

Esto imprimirá en la consola: "HOLA MUNDO". También existe la función "ToLower()" que convierte todos los caracteres a minúsculas, en caso de que sea necesario.

Otra opción es usar la función "CultureInfo" que permite elegir el idioma para la capitalización de la cadena. Por ejemplo, si queremos capitalizar una cadena en español, podemos usar el código "es-ES" dentro de la función.

```C#
string cadena = "hola mundo";
string capitalizada = cadena.ToUpper(new CultureInfo("es-ES"));
Console.Write(capitalizada);
```

## Profundizando en la capitalización de cadenas en C#

Además de la función "ToUpper()", también existen otras maneras de capitalizar una cadena en C#. Por ejemplo, se puede utilizar la función "TextInfo.ToTitleCase()" para capitalizar cada palabra de una cadena, en lugar de solo la primera letra.

```C#
using System.Globalization;
string cadena = "hola mundo";
TextInfo textInfo = new CultureInfo("es-ES").TextInfo;
string capitalizada = textInfo.ToTitleCase(cadena);
Console.WriteLine(capitalizada);
```

Esto imprimirá en la consola: "Hola Mundo". También se pueden usar expresiones regulares para capitalizar de manera más precisa según las necesidades del usuario.

## Ver también

- [Documentación oficial de C# sobre capitalizar cadenas](https://docs.microsoft.com/es-es/dotnet/api/system.string.toupper?view=net-5.0)
- [Expresiones regulares en C#](https://www.c-sharpcorner.com/UploadFile/b942f9/expresiones-regulares-en-c-sharp/)
- [CultureInfo en C#](https://www.c-sharpcorner.com/UploadFile/87b416/culture-specific-methods-in-C-Sharp/)