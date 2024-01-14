---
title:    "C#: Convirtiendo una cadena a minúsculas"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Por qué

Convertir una cadena a minúsculas es una tarea común en la programación. Al hacerlo, podríamos estar buscando una coincidencia de cadenas sin importar si están en mayúsculas o minúsculas, o simplemente queremos asegurarnos de que toda la cadena esté en el mismo formato antes de realizar operaciones en ella.

## Cómo hacerlo
Para convertir una cadena a minúsculas en C#, podemos hacer uso del método `ToLower()` de la clase `string`. Este método devuelve una nueva cadena con todos los caracteres en minúsculas.

```C#
string cadena = "HOLA AMIGOS";
string cadenaEnMinusculas = cadena.ToLower();

Console.WriteLine(cadenaEnMinusculas);
// Output: hola amigos
```

También podemos usar la sobrecarga del método `ToLower()` que toma como argumento un `CultureInfo` para especificar el idioma en el que queremos que se convierta la cadena. Por ejemplo, si queremos convertir una cadena en inglés a minúsculas en español, podemos hacer lo siguiente:

```C#
string cadena = "HELLO FRIENDS";
string cadenaEnMinusculas = cadena.ToLower(new CultureInfo("es-ES"));

Console.WriteLine(cadenaEnMinusculas);
// Output: hello friends
```

## Profundizando
Es importante tener en cuenta que el método `ToLower()` de la clase `string` solo convertirá letras en mayúsculas, dejando intactos los caracteres no alfabéticos y los caracteres especiales. Si queremos convertir una cadena completamente a minúsculas, incluyendo esos caracteres, podemos hacer uso del método `InvariantCulture.ToLower()`:

```C#
string cadena = "HELLO, HOW ARE YOU?";
string cadenaEnMinusculas = cadena.ToLowerInvariant();

Console.WriteLine(cadenaEnMinusculas);
// Output: hello, how are you?
```

También es importante tener en cuenta que el método `ToLower()` no modifica la cadena original, sino que devuelve una nueva cadena en minúsculas. Por lo tanto, es necesario asignar el resultado a una variable si queremos utilizarla posteriormente.

## Ver también
- [Método ToLower() de la clase string en la documentación de Microsoft](https://docs.microsoft.com/es-es/dotnet/api/system.string.tolower)
- [CultureInfo en la documentación de Microsoft](https://docs.microsoft.com/es-es/dotnet/api/system.globalization.cultureinfo?view=net-5.0)