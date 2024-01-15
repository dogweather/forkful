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

## Por qué

La conversión de una cadena de texto a minúsculas es una tarea común en la programación. Al convertir una cadena a minúsculas, podemos asegurar que nuestro programa sea más preciso y consistente al manejar entrada de usuarios.

## Cómo hacerlo

 Para convertir una cadena de texto a minúsculas en C#, podemos utilizar el método `ToLower()` de la clase `string`. Veamos un ejemplo de cómo podemos aplicar este método en nuestro código:

```C#
string texto = "Esto ES un Ejemplo";
Console.WriteLine(texto.ToLower());
```

El resultado de este código sería `esto es un ejemplo`, ya que hemos convertido todas las letras a minúsculas.

Si queremos convertir una cadena a minúsculas sin modificar la original, podemos usar el método `ToLowerInvariant()`. Este método es más recomendable ya que usa reglas de idioma neutras, evitando problemas con diferentes idiomas.

```C#
string texto = "ThIs iS a NoTher ExaMPle";
Console.WriteLine(texto.ToLowerInvariant());
```

El resultado de este código sería `this is a nother example`.

## Profundizando

La razón por la que usamos `ToLower()` o `ToLowerInvariant()` en lugar de simplemente cambiar manualmente todas las letras a minúsculas es porque estos métodos tienen en cuenta los caracteres acentuados y otros especiales. De esta manera, nuestro programa será más preciso al tratar diferentes idiomas.

Además, existe un tercer método llamado `ToLower(CultureInfo)`, el cual nos permite especificar un idioma específico para realizar la conversión.

## Ver también

- [Método ToLower en la documentación de Microsoft](https://docs.microsoft.com/es-es/dotnet/api/system.string.tolower?view=net-5.0)
- [Video tutorial sobre convertir cadenas a minúsculas en C#](https://www.youtube.com/watch?v=tKj5wJXFZPY)
- [Cómo convertir cadenas a mayúsculas en C#](https://www.geeksforgeeks.org/c-sharp-stringtolower-method/)