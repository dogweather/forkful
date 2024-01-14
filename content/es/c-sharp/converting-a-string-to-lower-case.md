---
title:                "C#: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¿Por qué convertir una cadena a minúsculas?

La conversión de un string a su forma en minúsculas puede ser útil en diferentes situaciones, como por ejemplo cuando se quiere comparar cadenas de manera insensible a mayúsculas y minúsculas, o cuando se desea mostrar una cadena en un formato uniforme, sin importar cómo fue ingresada por el usuario.

## Cómo hacerlo

```C#
//Ejemplo 1: Convertir una cadena a minúsculas

string cadena = "Mi Cadena en Mayúsculas";
string cadenaEnMinusculas = cadena.ToLower();

Console.WriteLine(cadenaEnMinusculas);

//Salida: mi cadena en mayúsculas
```

```C#
//Ejemplo 2: Comparar cadenas de manera insensible a mayúsculas y minúsculas

string cadena1 = "Casa";
string cadena2 = "casa";

console.WriteLine(cadena1.Equals(cadena2, StringComparison.OrdinalIgnoreCase));

//Salida: True
```

## Analizando en profundidad

La conversión de una cadena a minúsculas se realiza a través del método `ToLower()` de la clase `String`. Este método utiliza las reglas de localización del sistema operativo para determinar cómo convertir las letras mayúsculas a minúsculas. Por ejemplo, en español la letra "Ñ" se convertirá a "ñ" mientras que en inglés se seguirá utilizando la misma letra "N".

Además, es importante tener en cuenta que la conversión a minúsculas de una cadena puede afectar su longitud, ya que algunos caracteres mayúsculas tienen su equivalente en minúsculas con más de un byte.

## Ver también

- [Método `ToLower()` en la documentación de Microsoft](https://docs.microsoft.com/es-es/dotnet/api/system.string.tolower)
- [Método `Equals()` en la documentación de Microsoft](https://docs.microsoft.com/es-es/dotnet/api/system.string.equals)
- [Cadenas en C#](https://docs.microsoft.com/es-es/dotnet/csharp/programming-guide/strings/)