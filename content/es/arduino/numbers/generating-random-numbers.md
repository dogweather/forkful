---
date: 2024-01-27 20:32:31.200700-07:00
description: "Generar n\xFAmeros aleatorios en proyectos de Arduino involucra producir\
  \ valores que son impredecibles por dise\xF1o, crucial para aplicaciones como juegos,\u2026"
lastmod: '2024-03-11T00:14:33.151952-06:00'
model: gpt-4-0125-preview
summary: "Generar n\xFAmeros aleatorios en proyectos de Arduino involucra producir\
  \ valores que son impredecibles por dise\xF1o, crucial para aplicaciones como juegos,\u2026"
title: "Generaci\xF3n de n\xFAmeros aleatorios"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Generar números aleatorios en proyectos de Arduino involucra producir valores que son impredecibles por diseño, crucial para aplicaciones como juegos, simulaciones y sistemas de seguridad. Los programadores utilizan esta técnica para introducir variabilidad o tomar decisiones que no deberían ser deterministas.

## Cómo hacerlo:
Arduino proporciona funciones sencillas para generar números aleatorios: `randomSeed()` y `random()`. Para comenzar, inicializa el generador de números aleatorios para asegurar diferentes secuencias de números cada vez que tu programa se ejecute. Un enfoque a menudo utilizado es sembrar con una lectura analógica de un pin no conectado.

```Arduino
void setup() {
  Serial.begin(9600);
  // Inicializa la semilla aleatoria
  randomSeed(analogRead(0));
}

void loop() {
  // Genera un número aleatorio entre 0 y 99
  int randomNumber = random(100);
  Serial.println(randomNumber);
  delay(1000); // Retraso de un segundo para la legibilidad de la salida
}
```

El programa anterior inicializa el generador de números aleatorios en la función `setup()` y genera un nuevo número entre 0 y 99 en cada iteración del bucle, sacando el número al Monitor Serie.

Ejemplo de salida:
```
42
17
93
...
```

## Más Detalles
La función `random()` de Arduino, bajo el capó, utiliza un generador de números pseudoaleatorios (PRNG), que sigue una secuencia determinista pero parece estadísticamente aleatoria. El valor inicial, o semilla, de la secuencia influye mucho en su imprevisibilidad, de ahí el uso común de `randomSeed()` con una entrada algo aleatoria como punto de partida. Es importante notar que la aleatoriedad generada por Arduino es suficiente para la mayoría de los proyectos de aficionados pero puede no cumplir con los criterios para aplicaciones de alta seguridad debido a su previsibilidad con el tiempo. Para propósitos criptográficos, es aconsejable investigar algoritmos más sofisticados y generadores de números aleatorios de hardware (HRNG), que pueden proporcionar verdadera aleatoriedad al utilizar procesos físicos.
