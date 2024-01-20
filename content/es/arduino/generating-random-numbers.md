---
title:                "Generando números aleatorios"
html_title:           "Arduino: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?
Generar números aleatorios en la programación implica la creación de números que no siguen ningún patrón predecible. Los programadores utilizan esto para introducir elementos de imprevisibilidad, para pruebas y para simulaciones.

## Cómo hacerlo:
En Arduino, generamos números aleatorios utilizando la función `random()`.

```Arduino
void setup() {
  Serial.begin(9600);
  randomSeed(analogRead(0));
}

void loop() {
  Serial.println(random(100)); // esto generará un número aleatorio entre 0 y 99
  delay(1000);
}
```

Output Brut:

```
23
87
4
55
...
```

## Profundizando:
La función `random()` en Arduino no produce verdaderos números aleatorios, sino una secuencia de números que solo parece aleatoria si no conoces la secuencia inicial (aleatoria). La semilla para esta secuencia puede ser establecida con `randomSeed()`. Si no estableces una semilla, Arduino utilizará una semilla predeterminada.

Historicamente, la generación de números aleatorios ha sido un desafío. Alternativas a `random()`, como los Generadores de Número Aleatorio Congruencial Lineal, ofrecen secuencias más largas antes de repetir su ciclo, pero requieren más recursos.

Detalles de implementación: `random()` en Arduino devuelve longs, números enteros que varían entre -2,147,483,648 y 2,147,483,647 o entre 0 y 4,294,967,295.

## Ver también:
Para entender más sobre `random()` y `randomSeed()`, consulta los siguientes recursos:

1. Documentación oficial de Arduino para `random()`: https://www.arduino.cc/reference/en/language/functions/random-numbers/random/
2. Documentación oficial de Arduino para `randomSeed()`: https://www.arduino.cc/reference/en/language/functions/random-numbers/randomseed/