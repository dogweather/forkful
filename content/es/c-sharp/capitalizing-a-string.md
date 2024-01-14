---
title:    "C#: Capitalizar una cadena"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Por qué

Capitalizar una cadena (string) es una tarea común en la programación. Puede ser necesario cuando se quieren mostrar datos en un formato más limpio y legible, o cuando se desea asegurar que el usuario ingrese datos en un formato específico. Es una habilidad básica que todo programador debería conocer.

## Cómo hacerlo

En C#, existen varias formas de capitalizar una cadena. Una de las formas más simples es utilizando el método `ToUpper()`, que convertirá todas las letras de la cadena a mayúsculas. Esto se logra asignando el resultado del método a una nueva variable o sobrescribiendo la variable original. Por ejemplo:

```C#
string cadena = "hola mundo";
string resultado = cadena.ToUpper(); // resultado = "HOLA MUNDO"
```

Otra forma es utilizando el método `ToLower()`, que convertirá todas las letras de la cadena a minúsculas. Esto se puede hacer de la misma manera que con `ToUpper()`. Por ejemplo:

```C#
string cadena = "HOLA MUNDO";
string resultado = cadena.ToLower(); // resultado = "hola mundo"
```

También existe el método `ToTitleCase()`, que capitalizará la primera letra de cada palabra en la cadena. Sin embargo, es importante tener en cuenta que este método solo funcionará correctamente en idiomas que sigan reglas estrictas de capitalización (como el español). Por ejemplo:

```C#
string cadena = "este es un ejemplo";
string resultado = CultureInfo.CurrentCulture.TextInfo.ToTitleCase(cadena); // resultado = "Este Es Un Ejemplo"
```

## Profundizando

Si bien estas son las formas más básicas de capitalizar una cadena en C#, es importante tener en cuenta que existen muchas otras técnicas y métodos disponibles para lograr el mismo resultado. Se pueden utilizar expresiones regulares, manipulación de caracteres y muchas otras técnicas avanzadas.

Además, es importante recordar que la mayoría de estos métodos no cambian la cadena original, sino que devuelven una nueva cadena con el resultado. Por lo tanto, es importante asignar el resultado a una nueva variable o sobrescribir la original.

## Ver también

- [Método ToUpper() en MSDN](https://docs.microsoft.com/es-es/dotnet/api/system.string.toupper?view=netframework-4.7.2)
- [Método ToLower() en MSDN](https://docs.microsoft.com/es-es/dotnet/api/system.string.tolower?view=netframework-4.7.2)
- [Método ToTitleCase() en MSDN](https://docs.microsoft.com/es-es/dotnet/api/system.globalization.textinfo.totitlecase?view=netframework-4.7.2)