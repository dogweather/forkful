---
title:                "Generowanie liczb losowych"
html_title:           "Gleam: Generowanie liczb losowych"
simple_title:         "Generowanie liczb losowych"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Generowanie liczb losowych to proces tworzenia liczb, które nie mają przewidywalnej sekwencji lub wzoru. Programiści generują liczby losowe, aby wprowadzić element nieprzewidywalności w swoje projekty, jak gry komputerowe lub symulacje.

## Jak to zrobić:

Generowanie liczb losowych w Arduino jest proste. Poniższy kod generuje losowe liczby w zakresie od 0 do 100:

```Arduino
void setup(){
  Serial.begin(9600);
  randomSeed(analogRead(0)); 
}

void loop(){
  int randomNumber = random(0,100);
  Serial.println(randomNumber);
  delay(1000);
}
```
Kod ten wypisuje co sekundę losową liczbę w przedziale 0-100.

## Dogłębnie:

Historia generowania liczb losowych w komputerach sięga lat siedemdziesiątych XX wieku, kiedy to po raz pierwszy użyto algorytmów do generowania pseudolosowych ciągów liczb. O alternatywach dla funkcji `random()` na Arduino warto wspomnieć o generowaniu liczb losowych przy użyciu szumu termicznego, co zapewnia lepszy poziom losowości, ale jest skomplikowane do implementacji. Jeżeli chodzi o `random()`, jest to jedna z najprostszych opcji dostępnych dla Arduino i dla większości zastosowań jest wystarczająco dobry.

## Zobacz wiele innych:

1. Więcej o generowaniu liczb losowych: [Kurs Arduino] (https://create.arduino.cc/projecthub/arjun/generating-random-numbers-in-arduino-a-simple-guide-ec0665)

2. Szum termiczny jako źródło losowości: [Szum termiczny] (https://playground.arduino.cc/Main/TrueRandom/)

3. Inne metody generowania liczb losowych: [Metody generowania liczb losowych] (https://www.geekhideout.com/arduino-rand.shtml)