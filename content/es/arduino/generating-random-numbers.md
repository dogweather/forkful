---
title:    "Arduino: Generación de números aleatorios"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Por qué

La generación de números aleatorios es una habilidad útil para cualquier programador de Arduino. Permite crear programas que tomen decisiones al azar, agregando un nivel de complejidad e imprevisibilidad a sus proyectos.

## Cómo

Para generar números aleatorios en Arduino, puedes utilizar la función `random()`, que devuelve un número entero aleatorio entre 0 y el número especificado como parámetro. Por ejemplo, si queremos generar un número aleatorio entre 1 y 10, podemos usar `random(10) + 1`.

```Arduino
// Generar un número aleatorio entre 1 y 10
int numero_aleatorio = random(10) + 1;
Serial.println(numero_aleatorio);
```

También puedes usar `random()` con dos parámetros, para especificar un rango de números. Por ejemplo, `random(5, 10)` generará un número aleatorio entre 5 y 10.

```Arduino
// Generar un número aleatorio entre 5 y 10
int numero_aleatorio = random(5, 10);
Serial.println(numero_aleatorio);
```

Si necesitas generar un número aleatorio con decimales, puedes usar `random()` junto con `float()`, que convierte el número en un valor flotante.

```Arduino
// Generar un número aleatorio con decimales
float numero_aleatorio = float(random(10)) / 10;
Serial.println(numero_aleatorio, 2);
```

## Deep Dive

La función `random()` en realidad no genera números aleatorios verdaderos, ya que siempre se basa en el mismo algoritmo. Pero como el resultado es impredecible para los seres humanos, se considera "suficientemente aleatorio" para la mayoría de los casos.

Sin embargo, si necesitas una mayor aleatoriedad, puedes utilizar un módulo de hardware externo, como un sensor de luz o de temperatura, para generar números aleatorios verdaderos.

Otra técnica es usar una semilla o "seed" para la función `random()`. Esto puede ser un número que cambia constantemente, como la lectura de un pin analógico, lo que hace que los números generados sean menos predecibles.

## See Also

- [Documentación de Arduino sobre `random()`](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)
- [Artículo sobre semillas para generar números aleatorios en Arduino](https://arduinobasics.blogspot.com/2013/02/random-numbers-from-analogue-pin.html)
- [Tutorial sobre cómo utilizar un sensor de luz para generar números aleatorios en Arduino](https://www.instructables.com/id/Random-Number-generator-using-Arduino-and-LDR/)