---
date: 2024-01-20 17:38:26.948599-07:00
description: "C\xF3mo hacerlo: Usando C#, convierte un `string` a min\xFAsculas con\
  \ el m\xE9todo `.ToLower()` o `.ToLowerInvariant()`. Aqu\xED van ejemplos."
lastmod: '2024-03-13T22:44:59.065743-06:00'
model: gpt-4-1106-preview
summary: "Usando C#, convierte un `string` a min\xFAsculas con el m\xE9todo `.ToLower()`\
  \ o `.ToLowerInvariant()`."
title: "Conversi\xF3n de una cadena de texto a min\xFAsculas"
weight: 4
---

## Cómo hacerlo:
Usando C#, convierte un `string` a minúsculas con el método `.ToLower()` o `.ToLowerInvariant()`. Aquí van ejemplos:

```C#
string mensaje = "¡Hola Mundo!";
string mensajeEnMinusculas = mensaje.ToLower();
Console.WriteLine(mensajeEnMinusculas); // ¡hola mundo!

// Usando ToLowerInvariant para una conversión más consistente a nivel internacional
string mensajeInternacional = "¡HOLA MUNDO!";
string mensajeMinusculasInternacional = mensajeInternacional.ToLowerInvariant();
Console.WriteLine(mensajeMinusculasInternacional); // ¡hola mundo!
```

## Inmersión Profunda:
Históricamente, convertir a minúsculas era esencial para que sistemas insensibles a mayúsculas pudieran manejar entradas de usuario consistentemente. `ToLower()` utiliza las reglas de cultura de máquina, lo cual es útil para aplicaciones localizadas. `ToLowerInvariant()` se usa cuando deseas que el resultado sea independiente de la cultura, lo que es crucial en protocolos, almacenamiento de datos y cuando trabajas con varios idiomas.

Los métodos alternativos, como un mapeo personalizado de caracteres o el uso de expresiones regulares, existen pero rara vez se necesitan debido a la eficiencia de los métodos integrados.

Una consideración adicional: Algunas culturas tienen reglas complejas de conversión de mayúsculas y minúsculas. Por ejemplo, el carácter `i` se convierte en `İ` (I con punto) al convertirlo a mayúsculas en turco, que al convertirse de nuevo a minúsculas debería ser `i`. Correcto manejo de estos casos es importante al internacionalizar aplicaciones.

## Ver También:
- Documentación oficial de `.ToLower()` en Microsoft Docs: [https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower)
- Documentación oficial de `.ToLowerInvariant()` en Microsoft Docs: [https://docs.microsoft.com/en-us/dotnet/api/system.string.tolowerinvariant](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolowerinvariant)
- Guía general sobre globalización y localización en C#: [https://docs.microsoft.com/en-us/dotnet/standard/globalization-localization/](https://docs.microsoft.com/en-us/dotnet/standard/globalization-localization/)
