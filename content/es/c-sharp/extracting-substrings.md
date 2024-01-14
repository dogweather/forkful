---
title:    "C#: Extrayendo subcadenas"
keywords: ["C#"]
---

{{< edit_this_page >}}

##¿Por qué deberías extraer subcadenas? 

En el mundo de la programación, a menudo nos encontramos con la necesidad de trabajar con cadenas de texto en lugar de números. Y a veces, solo necesitamos una parte específica de esa cadena. La extracción de subcadenas nos permite obtener solo la parte que necesitamos de una cadena más grande, lo que ahorra tiempo y mejora la eficiencia de nuestro código.

## Cómo extraer subcadenas en C#

```C#
// Creamos una cadena de ejemplo
string cadena = "Hola, este es un ejemplo de una cadena de texto";

// Extraemos una subcadena a partir del índice 6
string subcadena = cadena.Substring(6);

// Imprimimos el resultado
Console.WriteLine(subcadena);
// Salida: "es es un ejemplo de una cadena de texto"
```

En este ejemplo, utilizamos el método `Substring()` para extraer una subcadena de una cadena existente. Le proporcionamos un índice de punto de partida y la subcadena resultante incluirá todo lo que viene después de ese índice.

Pero, ¿qué pasa si solo queremos una parte específica de esa subcadena? Podemos proporcionar tanto un índice de punto de partida como un número de caracteres para limitar la subcadena resultante. Veamos otro ejemplo:

```C#
// Creamos otra cadena de ejemplo
string cadena2 = "¡Hola a todos, bienvenidos al mundo de la programación!";

// Extraemos una subcadena que comienza en el índice 15 y tiene 8 caracteres de largo
string subcadena2 = cadena2.Substring(15, 8);

// Imprimimos el resultado
Console.WriteLine(subcadena2);
// Salida: "bienveni"
```

En este caso, nuestra subcadena resultante será "bienveni", ya que comenzando en el índice 15, tomaremos en cuenta los siguientes 8 caracteres.

## Profundizando en la extracción de subcadenas

Además del método `Substring()`, también podemos utilizar el operador de acceso a través de corchetes (`[]`) para extraer subcadenas en C#. Por ejemplo:

```C#
// Creamos una tercera cadena de ejemplo
string cadena3 = "¡Esta es otra forma de extraer subcadenas en C#!";

// Extraemos una subcadena de 4 caracteres a partir del índice 9
string subcadena3 = cadena3[9..13];

// Imprimimos el resultado
Console.WriteLine(subcadena3);
// Salida: "otra"
```

Aquí, utilizamos el operador de acceso a través de corchetes y la sintaxis de alcance `[índiceInicio..índiceFinal]` para extraer la subcadena "otra" de nuestra cadena original.

En resumen, la extracción de subcadenas puede ahorrarnos tiempo y esfuerzo en nuestro código al permitirnos trabajar con partes específicas de cadenas de texto en lugar de la cadena completa. Con los métodos `Substring()` y el operador de acceso a través de corchetes, tenemos varias opciones para lograr esto en C#.

##Ver también

- [Documentación de Microsoft sobre el método Substring en C#](https://docs.microsoft.com/es-es/dotnet/api/system.string.substring?view=netcore-3.1)
- [Tutorial sobre la extracción de subcadenas en C#](https://www.c-sharpcorner.com/article/c-sharp-string-operations-using-substring/)
- [Artículo sobre la manipulación de cadenas en C#](https://www.c-sharpcorner.com/article/working-with-strings-in-C-Sharp-part-1/)