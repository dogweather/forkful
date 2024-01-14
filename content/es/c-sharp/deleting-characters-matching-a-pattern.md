---
title:    "C#: Eliminando caracteres que coinciden con un patrón"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Por qué

A veces, durante la programación, nos encontramos con la necesidad de eliminar ciertos caracteres que coinciden con un patrón específico. Esto puede ser útil en situaciones donde queremos limpiar una cadena de texto o filtrar ciertos datos.

## Cómo hacerlo

Para eliminar caracteres que coinciden con un patrón en C#, podemos utilizar la función `Regex.Replace()`. Esta función toma tres parámetros: la cadena de texto original, el patrón de caracteres que queremos eliminar y el nuevo valor por el cual queremos reemplazarlos.

Por ejemplo, si queremos eliminar todos los números de una cadena de texto, podemos utilizar el siguiente código:

```C#
string texto = "Hola123mundo456";
string patron = "[0-9]";
string nuevoValor = "";

string resultado = Regex.Replace(texto, patron, nuevoValor);

Console.WriteLine(resultado); // Resultado: Hola mundo
```

En este código, utilizamos una expresión regular para el patrón, que en este caso es `[0-9]`, que representa cualquier número del 0 al 9. El nuevo valor es una cadena vacía, lo que significa que vamos a reemplazar cada número con una cadena vacía, básicamente eliminándolos de la cadena original.

Otro ejemplo podría ser eliminar todos los símbolos de puntuación de una cadena de texto:

```C#
string texto = "¡Hola, mundo!";
string patron = "[^a-zA-Z0-9]";
string nuevoValor = "";

string resultado = Regex.Replace(texto, patron, nuevoValor);

Console.WriteLine(resultado); // Resultado: Hola mundo
```

En este caso, utilizamos una expresión regular que excluye a todas las letras y números, y por lo tanto, solo coincidirá con los símbolos de puntuación. Esto significa que, al reemplazarlos con una cadena vacía, eliminamos todos los símbolos de puntuación de la cadena original.

## Profundizando

La función `Regex.Replace()` puede ser muy útil para eliminar caracteres en diferentes situaciones, pero también puede ser un poco más complicada que simplemente eliminar números o símbolos de puntuación. Esto se debe a que las expresiones regulares son una herramienta muy poderosa para buscar patrones en una cadena de texto, y pueden ser utilizadas para encontrar cualquier tipo de caracteres, no solo números o símbolos de puntuación.

Por ejemplo, podríamos usar una expresión regular para eliminar todas las letras minúsculas de una cadena de texto, solo dejando las letras mayúsculas:

```C#
string texto = "Hola Mundo";
string patron = "[a-z]";
string nuevoValor = "";

string resultado = Regex.Replace(texto, patron, nuevoValor);

Console.WriteLine(resultado); // Resultado: HM
```

Esto se logra mediante el uso del rango `[a-z]` en la expresión regular, que coincide con todas las letras minúsculas. Al reemplazarlas con una cadena vacía, solo nos quedamos con las letras mayúsculas en la cadena resultante.

También podemos utilizar expresiones regulares más complejas, con combinaciones de rangos y caracteres especiales, para lograr patrones de eliminación más específicos. Sin embargo, esto puede ser bastante avanzado y requiere un buen conocimiento de expresiones regulares.

## Ver también

- [Documentación de Microsoft sobre el método Regex.Replace()](https://docs.microsoft.com/es-es/dotnet/api/system.text.regularexpressions.regex.replace)
- [Tutorial de expresiones regulares en C#](https://www.c-sharpcorner.com/UploadFile/eab707/regular-expression-in-C-Sharp/)
- [Expresiones regulares en línea](https://regex101.com/) (útil para probar y entender cómo funcionan las expresiones regulares)