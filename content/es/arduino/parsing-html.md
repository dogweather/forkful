---
title:                "Arduino: Analizando html"
simple_title:         "Analizando html"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/parsing-html.md"
---

{{< edit_this_page >}}

## Por qué 

Muchas veces, al trabajar con páginas web, necesitamos extraer datos específicos de su código HTML para utilizarlos en nuestras aplicaciones. Aquí es donde entra en juego la técnica de parsear HTML. Parsear HTML es el proceso de analizar y extraer información de un documento HTML. En este blog post, exploraremos cómo podemos hacer esto utilizando Arduino.

## Cómo hacerlo

Para parsear HTML en Arduino, necesitamos seguir los siguientes pasos:

- Paso 1: Instalar la biblioteca "HTMLParser" en Arduino IDE.
- Paso 2: Incluir la biblioteca en nuestro código utilizando la sentencia ```#include <HTMLParser.h> ```
- Paso 3: Definir una variable de tipo ```HTMLParser```.
- Paso 4: Utilizar la función ```parse()``` para analizar y extraer información de nuestro código HTML.
- Paso 5: Utilizar las funciones ```parseNext()```, ```parseNextChar()``` y ```readTag()``` para obtener datos específicos.

Veamos un ejemplo de cómo podemos utilizar estas funciones para extraer el título de una página web:

```Arduino
#include <HTMLParser.h>

void setup() {
  HTMLParser parser;  //Definimos nuestra variable parser
  parser.parse("https://www.miweb.com"); //Analizamos el código HTML de la página web
  Serial.begin(9600);
  Serial.println(parser.parseNext("title")); //Imprimimos el título en el monitor serial
}

void loop() {
  // no hay código adicional en este ejemplo
}
```

En este ejemplo, utilizamos la función ```parseNext()``` para encontrar la etiqueta "title" en el código HTML y luego imprimimos su contenido en el monitor serial.

La salida en el monitor serial sería algo como esto:

> Título de la página

## Profundizando

El proceso de parsear HTML puede ser más complejo dependiendo de la estructura y el contenido del código HTML que estemos analizando. La biblioteca "HTMLParser" en Arduino ofrece muchas más funciones que podemos utilizar para obtener información específica de diferentes etiquetas o elementos en el código HTML.

Por ejemplo, podemos utilizar la función ```nextTag()``` para saltar de una etiqueta a otra o la función ```isOpeningTag()``` para verificar si una etiqueta es de apertura o de cierre. También podemos utilizar expresiones regulares para obtener datos más complejos.

Es importante tener en cuenta que, al igual que con cualquier otra técnica, parsear HTML utilizando Arduino puede tener sus limitaciones y debemos ser cuidadosos al manejar datos grandes o complejos.

## Ver también

Si quieres profundizar más en la técnica de parsear HTML en Arduino, te recomiendo revisar los siguientes enlaces:

- [Documentación de la biblioteca "HTMLParser"](https://github.com/adafruit/Adafruit_NeoPixel)
- [Tutorial de Sparkfun sobre parseo de código HTML en Arduino](https://learn.sparkfun.com/tutorials/html-parsing-with-the-htmlparser-library/all)
- [Ejemplos de código para parsear HTML en Arduino](https://www.hackster.io/search?i=projects&q=html+parser+arduino)