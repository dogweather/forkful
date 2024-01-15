---
title:                "Rozpoczynanie nowego projektu"
html_title:           "Arduino: Rozpoczynanie nowego projektu"
simple_title:         "Rozpoczynanie nowego projektu"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś początkującym w programowaniu i chcesz dowiedzieć się czegoś nowego, Arduino jest idealnym sposobem na rozpoczęcie. Może też być przydatne dla doświadczonych programistów, którzy chcą rozwijać swoje umiejętności w obszarze programowania urządzeń fizycznych.

## Jak to zrobić

```Arduino
void setup() {
    pinMode(LED_BUILTIN, OUTPUT);
}

void loop() {
    digitalWrite(LED_BUILTIN, HIGH);
    delay(500);
    digitalWrite(LED_BUILTIN, LOW);
    delay(500);
}
```

W tym prostym przykładzie wykorzystujemy wbudowane diody LED do stworzenia efektu migotania. Najpierw musimy ustawić pin diody jako wyjście, a następnie w pętli nieskończonej przerywać prąd, aby dioda mogła migać. Możesz eksperymentować z różnymi czasami opóźnień, aby uzyskać różne efekty.

## Dogłębna analiza

Arduino to platforma programistyczna, która łączy łatwy w użyciu język programowania z fizycznymi urządzeniami elektronicznymi. Zwykle rozpoczynamy od napisania programu w języku C++ w środowisku Arduino IDE, a następnie wgrywamy go na fizyczny mikrokontroler, który może być podłączony do różnych czujników i urządzeń.

## Zobacz też

- [Oficjalna strona Arduino](https://www.arduino.cc/)
- [Podstawy programowania w Arduino](https://tutorial.arduino.cc/)
- [Poradniki i projekty dla początkujących](https://www.arduinoprojects.net.pl/category/cwiczenia/)