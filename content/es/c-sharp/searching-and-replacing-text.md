---
title:    "C#: Buscando y reemplazando texto"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

##Por qué

En el mundo de la programación, a menudo nos encontramos con la necesidad de realizar cambios en grandes cantidades de texto. Ya sea para corregir errores, actualizar información o simplemente para hacer una tarea más eficiente, la búsqueda y reemplazo de texto es una habilidad esencial que todo programador debe tener en su arsenal.

##Cómo hacerlo

La búsqueda y reemplazo de texto es una tarea sencilla, pero muy útil. En C#, hay varias formas de lograrlo, pero aquí te mostraremos una de las más comunes utilizando expresiones regulares.

Primero, importaremos el namespace System.Text.RegularExpressions, donde se encuentran las herramientas necesarias para trabajar con expresiones regulares:

```C#
using System.Text.RegularExpressions;
```

Luego, declararemos nuestra cadena de texto donde realizaremos la búsqueda y reemplazo. En este caso, utilizaremos una dirección de correo electrónico como ejemplo:

```C#
string correo = "ejemplo@dominio.com";
```

Ahora, crearemos una expresión regular que nos permita encontrar y reemplazar el dominio de correo electrónico por uno nuevo. En este caso, utilizaremos una variable para almacenar el dominio original y otra para almacenar el nuevo dominio:

```C#
string dominioOriginal = "@dominio.com";
string nuevoDominio = "@nuevodominio.com";
```

Una vez que tengamos nuestras variables preparadas, podemos utilizar el método Replace de la clase Regex para realizar el cambio en nuestra cadena de texto:

```C#
correo = Regex.Replace(correo, dominioOriginal, nuevoDominio);
```

¡Y listo! Nuestra dirección de correo electrónico ahora tiene un nuevo dominio. El resultado final será "ejemplo@nuevodominio.com". Puedes jugar con diferentes expresiones regulares y patrones de reemplazo para realizar cambios más complejos en tus cadenas de texto.

##Profundizando

La utilización de expresiones regulares para buscar y reemplazar texto nos da una gran flexibilidad y nos permite realizar cambios en grandes cantidades de datos de una manera más eficiente. Existen muchas herramientas y técnicas que se pueden utilizar para trabajar con expresiones regulares en C#, así que te animamos a investigar y seguir aprendiendo sobre este tema.

##Ver también

- [Documentación de Microsoft sobre expresiones regulares en C#](https://docs.microsoft.com/es-es/dotnet/standard/base-types/regular-expression-language-quick-reference)
- [Expresiones regulares en C# para principiantes](https://www.tutorialspoint.com/regular-expression-in-chash)
- [Expresiones regulares en C# para buscar y reemplazar texto](https://www.codeproject.com/Articles/20348/Regular-Expressions-with-NET)