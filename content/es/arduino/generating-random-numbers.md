---
title:                "Generando números aleatorios"
date:                  2024-01-20T17:48:35.719620-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generando números aleatorios"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?

Generar números aleatorios es el proceso de crear una secuencia de números que no sigue un patrón predecible. Los programadores usan números aleatorios para simular incertidumbre, probar algoritmos y añadir variabilidad a juegos y aplicaciones.

## Cómo:

```Arduino
void setup() {
  Serial.begin(9600);
  randomSeed(analogRead(0)); // Inicializa la semilla del generador con un valor leído de un pin analógico no utilizado
}

void loop() {
  int numeroAleatorio = random(100); // Genera un número aleatorio entre 0 y 99
  Serial.println(numeroAleatorio);
  delay(1000); // Espera 1 segundo antes de generar otro número
}
```

Salida de muestra:
```
23
58
77
42
...
```

## Inmersión Profunda:

Históricamente, generar números verdaderamente aleatorios ha sido un desafío. Los métodos primitivos incluían el lanzamiento de dados o monedas. En electrónica y computación, se utilizan algoritmos de números pseudoaleatorios (PRNG) que necesitan una 'semilla' para iniciar la secuencia.

Las alternativas al `random()` de Arduino incluyen el uso de fuentes de entropía externa o complejos algoritmos de software. Pero para propósitos generales en un Arduino, random() es suficiente.

El PRNG de Arduino `random()` no es criptográficamente seguro, por lo que no se debe utilizar para seguridad o cifrado. Además, la función `randomSeed()` se utiliza para variar la secuencia de números aleatorios empezando con una semilla diferente; si no usas `randomSeed()`, tu Arduino comenzará con la misma secuencia de números aleatorios cada vez que se reinicie.

## Ver También:

- `randomSeed()`: https://www.arduino.cc/reference/en/language/functions/random-numbers/randomseed/
- Generación de números aleatorios: https://es.wikipedia.org/wiki/Generador_de_números_aleatorios
- Función `analogRead()`: https://www.arduino.cc/reference/en/language/functions/analog-io/analogread/
