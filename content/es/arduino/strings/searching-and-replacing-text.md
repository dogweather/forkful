---
title:                "Buscando y reemplazando texto"
date:                  2024-01-20T17:57:10.748664-07:00
model:                 gpt-4-1106-preview
simple_title:         "Buscando y reemplazando texto"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Buscar y reemplazar texto en la programación de Arduino es esencial para editar cadenas, como cambiar una palabra o frase por otra. Esta técnica es útil para corregir errores, actualizar información o modificar mensajes para diferentes situaciones.

## Cómo hacerlo:

Para buscar y reemplazar texto en Arduino, no hay una función incorporada que lo haga directamente, pero podemos diseñar una fácilmente. Aquí tienes un ejemplo de cómo buscar y reemplazar palabras en una cadena:

```Arduino
String buscarYReemplazar(String cadena, String aBuscar, String reemplazo) {
  int posicion = cadena.indexOf(aBuscar);
  while (posicion != -1) {
    cadena.replace(aBuscar, reemplazo);
    posicion = cadena.indexOf(aBuscar, posicion + reemplazo.length());
  }
  return cadena;
}

void setup() {
  Serial.begin(9600);
  String texto = "Hola mundo. Arduino es genial. El mundo de Arduino es amplio.";
  String textoModificado = buscarYReemplazar(texto, "mundo", "universo");
  Serial.println(textoModificado);
}

void loop() {
  // no hay código aquí
}
```

Cuando cargues este código, deberías ver en el Monitor Serial el siguiente texto:

`Hola universo. Arduino es genial. El universo de Arduino es amplio.`

## Inmersión profunda:

Buscar y reemplazar texto no es algo nativo en el lenguaje de programación de Arduino, que es una variante de C++. Sin embargo, con el paso del tiempo los programadores han creado soluciones como la función anterior, aprovechando las capacidades de las cadenas de texto (`String`). Alternativas incluyen manipular los arrays de caracteres (`char` arrays), pero trabajar con `String` ofrece una sintaxis más directa y menos propensa a errores, especialmente para principiantes.

Ten en cuenta que el uso excesivo del tipo `String` puede llevar a la fragmentación de la memoria en proyectos más grandes debido al manejo dinámico de la memoria en Arduino. En aplicaciones críticas, los arrays de caracteres estáticos son preferidos. Además, siempre es importante considerar el tamaño del texto y la memoria disponible, especialmente en dispositivos con recursos limitados como Arduino.

## Ver también:

- [Documentación oficial de Arduino - String](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [Tutorial de Arduino sobre Strings](https://www.arduino.cc/en/Tutorial/BuiltInExamples/StringAdditionOperator)
