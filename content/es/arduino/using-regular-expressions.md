---
title:                "Usando expresiones regulares"
html_title:           "Go: Usando expresiones regulares"
simple_title:         "Usando expresiones regulares"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Las expresiones regulares (regex) son patrones que ayudan a buscar, encontrar y manejar texto de una manera más eficiente. Los programadores de Arduino las usan para filtrar y analizar datos de manera eficiente.

## Cómo:
Usar regex en Arduino involucra principalmente dos funciones: `regexMatch()` y `regexSearch()`. Aquí hay un ejemplo del uso de `regexMatch()`:

```Arduino
#include <Regexp.h>

String patern = "[0-9]*"; //patrón que coincide con cualquier número
MatchState ms;
ms.Target ("123abc456");
char result = ms.Match ("[0-9]*");

if (result == REGEXP_MATCHED) {
  Serial.println ("Coincidencia encontrada");
}
```

La salida será "Coincidencia encontrada":

```Arduino
Coincidencia encontrada
```

La función `regexSearch()` busca patrones en un texto y los retorna, como en este ejemplo:

```Arduino
#include <Regexp.h>

String patern = "[a-z]*"; //Patrón que coincide con cualquier letra 
MatchState ms;
ms.Target ("123abc456");
char result = ms.Match ("[a-z]*");

if (result == REGEXP_MATCHED) {
  Serial.println ("Coincidencia encontrada: " + ms.GetMatch());
}
```

La salida será "Coincidencia encontrada: abc":

```Arduino
Coincidencia encontrada: abc
```
## Bajo la superficie

Regex fue inventado en los años 1950 por Stephen Cole Kleene como una forma de describir eventos en una notación llamada "álgebra de eventos". Hoy en día, la mayoría de los lenguajes de programación, incluido Arduino, soportan regex, aunque la implementación puede variar dependiendo del lenguaje.

Existen alternativas a regex, como el uso de funciones "indexOf" o "charAt", pero estas pueden no ser tan potentes o eficientes. Cuando se trabaja con grandes cantidades de datos o patrones más complejos, regex es a menudo la mejor elección.

## Ver también

Aquí tienes algunos enlaces para seguir aprendiendo sobre regex en Arduino:
- Documentación oficial de Arduino: https://www.arduino.cc/reference/en/
- Guía práctica de regex: https://www.regular-expressions.info/tutorial.html
- Herramienta para probar regex: https://regex101.com/