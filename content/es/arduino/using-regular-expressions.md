---
title:                "Uso de expresiones regulares"
date:                  2024-01-19
html_title:           "Arduino: Uso de expresiones regulares"
simple_title:         "Uso de expresiones regulares"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## ¿Qué es y por qué?
Las expresiones regulares son una herramienta poderosa para buscar y manipular texto siguiendo patrones definidos. Los programadores las utilizan para simplificar tareas complejas de búsqueda y validación en cadenas de caracteres.

## Cómo hacerlo:
En Arduino, no hay soporte de biblioteca estándar para expresiones regulares, pero podemos usar funciones básicas para tareas similares. Ejemplo: buscar si una cadena contiene una palabra.

```Arduino
String texto = "Hola Mundo";
if (texto.indexOf("Mundo") > -1) {
  Serial.println("Palabra encontrada!");
} else {
  Serial.println("Palabra no encontrada.");
}
```

Salida esperada:
```
Palabra encontrada!
```

Para tareas más complejas, se puede incluir la biblioteca `<Regex.h>`, disponible por terceros, y seguir sus propios ejemplos.

## Inmersión Profunda
Las expresiones regulares se han usado desde los años 1950. En muchos lenguajes de programación modernos, son una característica estándar. Sin embargo, en el entorno de Arduino, debido a la limitada memoria y capacidad de procesamiento, se usan en menor medida y con bibliotecas externas. Como alternativa a las expresiones regulares, a menudo se utilizan métodos de búsqueda y comparación de cadenas proporcionados por la clase `String`.

## Ver También
- Documentación de Arduino sobre la clase `String`: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- Una biblioteca de expresiones regulares para Arduino: https://github.com/nickgammon/Regexp
- Tutoriales de Arduino para principiantes: https://www.arduino.cc/en/Tutorial/BuiltInExamples
