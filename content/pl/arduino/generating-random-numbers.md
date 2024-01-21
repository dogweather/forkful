---
title:                "Generowanie liczb losowych"
date:                  2024-01-20T17:48:23.093785-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generowanie liczb losowych"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Co to jest generowanie losowych liczb? To po prostu sposób na otrzymanie liczby, która jest nieprzewidywalna. Programiści wykorzystują je do wszystkiego - od gier po symulacje i eksperymenty.

## How to:
Użycie funkcji `random()` w Arduino to bułka z masłem. Podstawmy to do kodu:

```Arduino
void setup() {
  Serial.begin(9600);        // Start the serial communication
  randomSeed(analogRead(0)); // Initialize random number generator
}

void loop() {
  int randomNumber = random(1, 100); // Generate a random number between 1 and 99
  Serial.println(randomNumber);       // Print the random number to the serial monitor
  delay(1000);                        // Wait for a second
}
```

Po zuploadowaniu kodu, otwórz Monitor Serialny. Pojawią się losowe liczby co sekundę.

## Deep Dive:
Generowanie losowych liczb w elektronice ma długą historię. Na początku stosowano wielkie, mechaniczne "maszyny losujące". Arduino, używając `randomSeed()`, inicjuje generator liczb pseudolosowych, co znaczy, że liczby wyglądają na losowe, ale bazują na określonym algorytmie.

Inne metody? Możesz użyć zewnętrznych czujników (jak termistor) do generowania ziarna dla lepszej losowości. Detale implementacji? Znaczące jest że funkcja `random()` bez `randomSeed()` wygeneruje ten sam ciąg liczb po każdym resecie Arduino.

## See Also:
Spragniony więcej? Zajrzyj:

- Dokumentacja Arduino o funkcji `random()`: https://www.arduino.cc/reference/en/language/functions/random-numbers/random/
- Dyskusja na forum Arduino o losowości i Ziarnach: https://forum.arduino.cc/index.php?topic=396450
- Rozważanie entropii i sprzętowych generatorów liczb losowych (HWRNGs): http://www.circuitbasics.com/arduino-random-numbers/