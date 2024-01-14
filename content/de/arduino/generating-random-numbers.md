---
title:    "Arduino: Generieren von Zufallszahlen"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Warum

Wer gerne mit Arduinos programmiert, wird oft vor der Herausforderung stehen, zufällige Zahlen zu generieren. Dies ist zum Beispiel bei Spielen oder Simulationen notwendig oder auch um eine gewisse Unvorhersehbarkeit in ein Programm zu integrieren. Im folgenden Artikel erfährst du, wie du mit dem Arduino zufällige Zahlen erzeugen kannst.

## Wie geht man vor?

Um zufällige Zahlen mit dem Arduino zu generieren, gibt es verschiedene Vorgehensweisen. Eine Möglichkeit ist die Verwendung der built-in Funktion "random()". Diese Funktion erzeugt eine Zufallszahl zwischen 0 und 32767. Im folgenden Beispiel werden 10 zufällige Zahlen erzeugt und ausgegeben:

```Arduino
void setup(){
  Serial.begin(9600);
}

void loop(){
  for(int i = 0; i < 10; i++){
    int randomNumber = random(32767);
    Serial.println(randomNumber);
  }
}
```

Die Ausgabe sieht dann folgendermaßen aus:

```
16661
20207
26962
2106
9243
18826
8862
28801
22949
31052
```

## Eintauchen in die Materie

Die Funktion "random()" basiert auf dem sogenannten Pseudo-Zufallszahlengenerator. Dieser Algorithmus erzeugt Zahlenfolgen, die auf den ersten Blick zufällig aussehen, jedoch in Wirklichkeit anhand eines Startwertes stets die gleiche Zahlenfolge generieren. Dabei ist die Qualität des Startwertes entscheidend für die Zufälligkeit der Zahlen. Um eine bessere Zufälligkeit zu erreichen, kann der Startwert zum Beispiel auf Basis von Sensordaten wie Rauschen oder Temperatur erzeugt werden.

## Siehe auch

- [Offizielle Arduino Referenz für die random() Funktion](https://www.arduino.cc/reference/de/language/functions/random-numbers/random/)
- [Erklärvideo zum Thema Zufallszahlen](https://www.youtube.com/watch?v=La7gRqJpsNA)
- [Random Number Generator Tutorial für den Arduino](https://digi.seil-software.com/how-to-create-random-numbers-with-an-arduino/)