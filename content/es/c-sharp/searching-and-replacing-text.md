---
title:                "Buscando y reemplazando texto"
html_title:           "C: Buscando y reemplazando texto"
simple_title:         "Buscando y reemplazando texto"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Buscar y Reemplazar Texto en C#: Lo Esencial

## ¿Qué y Por Qué?

Buscar y reemplazar texto es una operación común en el desarrollo de software que implica encontrar una cadena de texto específica (la cadena "buscada") y sustituirla por otra (la cadena "reemplazo"). Los programadores lo hacen para modificar datos, corregir errores o transformar formatos.

## Como Hacerlo:

Podemos usar la función `Replace()` de la clase `String` en C# para hacer esto de manera eficiente. Veamos un ejemplo:

```C#
string texto = "Hola Mundo!";
string textoModificado = texto.Replace("Hola", "Adiós");
Console.WriteLine(textoModificado);
```
Salida:

```C#
Adiós Mundo!
```
En el código anterior, estamos buscando la cadena "Hola" y la estamos reemplazando con "Adiós".

## Buceo Profundo:

### Contexto Histórico:
La función `Replace()` ha estado presente en C# desde sus primeros días, indicando la importancia que los desarrolladores le dan a esta funcionalidad.

### Alternativas:
Si bien `Replace()` es suficientemente potente para la mayoría de tareas, existen otras alternativas más flexibles como `Regex.Replace()` que permite usar expresiones regulares para buscar y reemplazar texto.

### Detalles de Implementación:
Internamente, `Replace()` utiliza un algoritmo que busca la cadena de texto “buscada” en cada posición del texto original hasta que encuentra una coincidencia.

## Ver También:

Para más detalles acerca de `String.Replace()`, puedes consultar la documentación oficial de Microsoft. [String.Replace Method](https://docs.microsoft.com/es-es/dotnet/api/system.string.replace?view=net-5.0)

Para aprender más acerca de RegEx y su método `Replace()`, puedes visitar este enlace: [Regex.Replace Method](https://docs.microsoft.com/es-es/dotnet/api/system.text.regularexpressions.regex.replace?view=net-5.0)