---
title:                "Interpolando una cadena de texto"
html_title:           "Haskell: Interpolando una cadena de texto"
simple_title:         "Interpolando una cadena de texto"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/interpolating-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

La interpolación de string es una forma de inyectar valores de variables directamente en una cadena de texto. Los programadores la usan para hacer el código más legible y para construir cadenas de texto de manera más eficiente.

## Cómo hacerlo:

Aquí te dejo un ejemplo de cómo usar la interpolación de string en C#.

```C#
string name = "Juan";
int age = 32;

//Interpolación de string
string sentence = $"Hola, {name}. Tienes {age} años.";
Console.WriteLine(sentence);
```
El output de este código será:

```C#
"Hola, Juan. Tienes 32 años."
```
## Buceo Profundo

La interpolación de string en C# se introdujo con la versión 6.0, reemplazando la antigua y menos eficiente función `string.Format()`. Alternativamente, aún puedes usar `string.Format()` o concatenación de string, pero la interpolación tiende a ser más legible y eficiente.

En cuanto a los detalles de implementación, cuando usas interpolación de string, el compilador traduce la expresión interpolada a una llamada a `string.Format()`. Por lo tanto, en tiempo de ejecución, no hay diferencia de rendimiento entre usar `string.Format()` o interpolación de string. Sin embargo, el beneficio se encuentra en la legibilidad del código y la eficiencia de la escritura.

## Más info

Para aprender más sobre la interpolación de string en C#, puedes consultar los siguientes enlaces:

- Documentación oficial de Microsoft: [String Interpolation](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/tokens/interpolated).