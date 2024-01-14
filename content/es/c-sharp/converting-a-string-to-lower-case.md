---
title:    "C#: Convirtiendo una cadena a minúsculas"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¿Por qué convertir una cadena a minúsculas?

Convertir una cadena de texto a minúsculas es una tarea común en la programación ya que permite estandarizar y manipular los datos de manera más eficiente. También puede ser útil para comparar cadenas de texto sin importar si están escritas en mayúsculas o minúsculas.

## Cómo hacerlo en C#

Para convertir una cadena de texto a minúsculas en C#, podemos utilizar el método `ToLower()` de la clase `String`. Este método toma la cadena original y devuelve una nueva cadena en minúsculas. Veamos un ejemplo:

```C#
string original = "Hola Mundo";
string minusculas = original.ToLower();

Console.WriteLine(minusculas);
// Salida: hola mundo
```

En este ejemplo, utilizamos el método `ToLower()` para convertir la cadena `original` en una nueva cadena en minúsculas llamada `minusculas`. Luego, imprimimos la nueva cadena en la consola y obtenemos "hola mundo" como resultado.

También podemos convertir una cadena a minúsculas utilizando el método `ToLowerInvariant()` de la clase `String`. La diferencia con el método anterior es que `ToLowerInvariant()` utiliza reglas de conversión invariantes al idioma y cultura, lo que puede ser útil en aplicaciones multilingües.

```C#
string original = "HELLO WORLD";
string minusculas = original.ToLowerInvariant();

Console.WriteLine(minusculas);
// Salida: hello world
```

Es importante tener en cuenta que tanto `ToLower()` como `ToLowerInvariant()` devuelven una nueva cadena en minúsculas y no modifican la cadena original. Si queremos guardar el resultado en la misma variable, podemos utilizar la asignación compuesta `+=`.

```C#
string cadena = "Hola a Todos";

cadena += cadena.ToLower();

Console.WriteLine(cadena);
// Salida: Hola a Todoshola a todos
```

## Un poco más sobre la conversión de cadenas a minúsculas

En C#, los métodos `ToLower()` y `ToLowerInvariant()` utilizan las reglas de conversión del idioma y cultura del sistema operativo. Esto significa que si ejecutamos nuestro código en un sistema con configuración de idioma y cultura diferente, los resultados pueden variar.

Además, al convertir una cadena a minúsculas no solo se convierten las letras de la A a la Z, sino que también se convierten otros caracteres especiales como "ñ" o "á". También se convierten los caracteres Unicode a su equivalente en minúsculas, lo que puede ser importante en aplicaciones que trabajan con idiomas que utilizan caracteres especiales.

## Ver también

- [Método ToLower() (Referencia de C#)](https://docs.microsoft.com/es-es/dotnet/api/system.string.tolower)
- [Método ToLowerInvariant() (Referencia de C#)](https://docs.microsoft.com/es-es/dotnet/api/system.string.tolowerinvariant)