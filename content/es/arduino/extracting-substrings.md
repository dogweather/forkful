---
title:                "Extrayendo subcadenas"
html_title:           "C: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Extraer subcadenas es el proceso de obtener una porción más pequeña de una cadena de caracteres. Los programadores lo hacen para manipular y analizar datos más eficazmente.

## Cómo hacerlo:

En Arduino, extrayendo subcadenas es simple. Aquí está un ejemplo:

```Arduino
String str = "Hola Mundo!";
String subStr = str.substring(5, 11);
Serial.begin(9600);
Serial.println(subStr);
```

Salida: 

```Arduino
Mundo!
```

En este código, `substring(5, 11)` extrae un fragmento de la cadena `str` desde el índice 5 al 10 (el índice 11 no se incluye).

## Análisis Adicional

1. **Historia**: Desde los inicios de la programación, la manipulación de cadenas ha sido esencial. La función `substring` ha facilitado esta tarea en múltiples lenguajes.

2. **Alternativas**: Además de `substring`, Arduino ofrece `charAt` para acceder a un carácter específico y `indexOf` para buscar cadenas.

3. **Detalles de Implementación**: Arduino implementa `substring` devolviendo un objeto `String` nuevo. Importante recordar que cada uso de `substring` consume memoria adicional.

## Ver También

1. Documentación de Arduino: [String.substring()](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/)
2. Foro de Arduino: [Trabajando con String](https://forum.arduino.cc/index.php?topic=128635.0)
3. Tutorial de Arduino: [Manipulación de cadenas de texto](https://www.prometec.net/arduino-string/)