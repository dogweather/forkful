---
title:    "C#: Usando expresiones regulares"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Por qué utilizar expresiones regulares en C#

Las expresiones regulares son una herramienta poderosa en el mundo de la programación. Nos permiten buscar y manipular patrones de texto de una manera rápida y eficiente. Si eres un programador de C#, definitivamente deberías considerar incorporar expresiones regulares en tus proyectos.

## Cómo utilizar expresiones regulares en C#

Para utilizar expresiones regulares en C#, primero debemos importar la clase `Regex` del espacio de nombres `System.Text.RegularExpressions`. Luego, podemos llamar al constructor de `Regex` y pasarle como argumento una cadena que represente el patrón que deseamos buscar. Por ejemplo:

```C#
Regex regex = new Regex("patrón");
```

Una vez que tenemos nuestro objeto `Regex` creado, podemos utilizar los siguientes métodos para buscar y manipular texto:

* `IsMatch(string input)`: devuelve `true` si la cadena `input` contiene una coincidencia con el patrón, o `false` si no hay coincidencia.
* `Match(string input)`: devuelve un objeto `Match` que representa la primera coincidencia encontrada en la cadena `input`.
* `Matches(string input)`: devuelve una colección de objetos `Match` que representan todas las coincidencias encontradas en la cadena `input`.
* `Replace(string input, string replacement)`: reemplaza todas las coincidencias en la cadena `input` con la cadena `replacement`.

Veamos un ejemplo de cómo utilizar expresiones regulares para validar un número telefónico en formato español:

```C#
string telefono = "+34 123456789";
Regex regex = new Regex(@"\+\d{2} \d{9}"); // El patrón consiste en un signo '+' seguido de dos dígitos, un espacio y 9 dígitos más.
bool esValido = regex.IsMatch(telefono); // Devuelve true si el número coincide con el patrón.
if (esValido)
{
    Console.WriteLine("El número de teléfono es válido.");
}
else
{
    Console.WriteLine("El número de teléfono no es válido.");
}
```

La salida de este código sería `El número de teléfono es válido.`

## Profundizando en expresiones regulares

Las expresiones regulares en C# tienen una sintaxis específica que nos permite buscar patrones con gran precisión. Aquí hay algunos conceptos importantes a tener en cuenta:

* **Caracteres especiales**: ciertos caracteres como `\`, `*` y `+` tienen un significado especial en expresiones regulares y deben ser precedidos por un carácter de escape `\` si queremos buscarlos en un texto.
* **Caracteres comodín**: los caracteres `.` y `*` se utilizan para representar cualquier carácter y cualquier cantidad de caracteres, respectivamente.
* **Grupos y anclas**: se pueden utilizar paréntesis para agrupar secciones de un patrón y utilizar anclas como `^` y `$` para indicar el comienzo y el final de una cadena.
* **Cuantificadores**: los símbolos `+`, `*` y `?` se utilizan para especificar la cantidad de veces que un patrón debe repetirse.

Para una lista completa de todos los elementos de sintaxis disponibles en expresiones regulares de C#, puedes consultar [la documentación oficial](https://docs.microsoft.com/es-es/dotnet/standard/base-types/regular-expression-language-quick-reference) de Microsoft.

## Ver también

* [Expresiones regulares en C#](https://docs.microsoft.com/es-es/dotnet/standard/base-types/regular-expression-language-quick-reference)
* [Documentación de la clase Regex en C#](https://docs.microsoft.com/es-es/dotnet/api/system.text.regularexpressions.regex?view=net-5.0)
* [Tutorial de expresiones regulares en C#](https://www.c-sharpcorner.com/article/regular-expression-in-c-sharp/)