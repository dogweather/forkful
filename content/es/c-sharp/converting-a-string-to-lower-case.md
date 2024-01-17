---
title:                "Convirtiendo una cadena a minúsculas"
html_title:           "C#: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

La conversión de una cadena (string) a minúsculas es el proceso de cambiar todas las letras a su forma más pequeña en una cadena de texto. Los programadores realizan esta acción para estandarizar la entrada de datos y facilitar la comparación y manipulación de cadenas.

## Cómo hacerlo:

Hay varias formas de convertir una cadena a minúsculas en C#, dependiendo de la estructura y necesidades de tu código. Algunas opciones incluyen el uso de la función integrada `ToLower()`, que convierte una cadena completa a minúsculas, o la función `ToLowerInvariant()`, que utiliza el alfabeto específico del idioma para convertir a minúsculas. Ejemplo:

```C#
string cadena = "HOLA MUNDO";
string cadenaMinusculas = cadena.ToLower(); // Resultado: hola mundo
string cadenaMinusculasInvariant = cadena.ToLowerInvariant(); // Resultado: hola mundo
```

También es posible usar la función `ToLower()` en una cadena parcial utilizando el método `Substring()` y la propiedad `Length` para especificar qué parte de la cadena se debe convertir. Ejemplo:

```C#
string cadena = "hOlA mUnDo";
string primeraParte = cadena.Substring(0, 2); // Resultado: hO
string primeraParteMinusculas = primeraParte.ToLower(); // Resultado: ho
```

## Profundizando:

La conversión de cadenas a minúsculas puede parecer un proceso simple, pero es importante tener en cuenta la compatibilidad con diferentes alfabetos y lenguajes. Además, existen otras opciones de manipulación de cadenas, como convertir a mayúsculas o capitalizar la primera letra. También es importante considerar el rendimiento, ya que algunas opciones pueden ser más eficientes que otras.

## Ver también:

- Documentación oficial de Microsoft sobre el método `ToLower()`: https://docs.microsoft.com/es-es/dotnet/api/system.string.tolower?view=net-5.0
- Ejemplos de uso de funciones de manipulación de cadenas en C#: https://www.flujo.net/blog/13-formas-de-convertir-string-en-minusculas-o-mayusculas-en-c/
- Lecciones sobre manipulación de cadenas en el curso gratuito de C# en Codecademy: https://www.codecademy.com/learn/learn-c-sharp/modules/learn-csharp-strings/