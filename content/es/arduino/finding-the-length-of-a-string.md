---
title:                "Encontrando la longitud de una cadena"
html_title:           "Arduino: Encontrando la longitud de una cadena"
simple_title:         "Encontrando la longitud de una cadena"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

---

## ¿Qué & Por qué?

Determinar la longitud de una cadena es contar el número de caracteres que contiene. Los programadores lo hacen para manipular y gestionar datos eficazmente.

## Cómo hacerlo:

El siguiente ejemplo muestra cómo encontrar la longitud de una cadena en Arduino:

```Arduino
String miCadena = "Hola Mundo";
int longitud = miCadena.length();
Serial.println(longitud);
```

Al ejecutar este código, el resultado sería `10`, que es la cantidad de caracteres en "Hola Mundo".

## Análisis en profundidad:

Historicamente, Arduino proporciona la función `length()` para calcular la longitud de una cadena enseguida. Sin embargo, hay alternativas como el bucle manual para contar los caracteres hasta que se encuentre el carácter nulo '\0', que señala el final de la cadena.

```Arduino
char miCadena[] = "Hola Mundo";
int longitud = 0;
while(miCadena[longitud] != '\0') {
    longitud++;
}
Serial.println(longitud);
```

Hoy en día, se prefiere la función `length()` porque es más sencilla y eficiente. Asegúrese de que la cadena sea del tipo String para utilizar esta función.

## Ver también:

Para obtener más detalles sobre la programación de Arduino, puedes visitar los siguientes enlaces:

- [Referencia oficial de Arduino](https://www.arduino.cc/reference/en/)
- [Documentación de la función length()](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/length/)