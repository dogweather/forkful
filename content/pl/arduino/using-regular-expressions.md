---
title:                "Arduino: Używanie wyrażeń regularnych"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach programowanie jest nieodłączną częścią naszego życia. Aby tworzyć skuteczne i wydajne kody, musimy korzystać z różnorodnych narzędzi. Jednym z nich są wyrażenia regularne, które mogą znacznie usprawnić nasze projekty Arduino.

## Jak to zrobić

Wyrażenia regularne są niczym innym jak wzorcami poszukiwań w tekście. Pozwalają odnaleźć i przetworzyć konkretne fragmenty tekstu, co ułatwia pracę z danymi. W Arduino możemy wykorzystać bibliotekę "Regex", aby korzystać z wyrażeń regularnych. Poniżej znajdują się przykłady kodów, które możesz wykorzystać w swoich projektach.

```
#include <Regex.h>  //zaimportowanie biblioteki

void setup() {
  Serial.begin(9600); //inicjalizacja portu szeregowego
}

void loop() {
  String str = "Ala ma kota"; //tekst do przetworzenia
  Regex reg = Regex("(.*) (.*) (.*)"); // wzorzec dla tekstu
  if (reg.match(str)) { //sprawdzenie, czy wzorzec pasuje do tekstu
    Serial.println(reg[0]); //wyświetlenie pełnego dopasowania
    Serial.println(reg[1]); //wyświetlenie pierwszej grupy dopasowania
    Serial.println(reg[2]); //wyświetlenie drugiej grupy dopasowania
    Serial.println(reg[3]); //wyświetlenie trzeciej grupy dopasowania
  }
  delay(1000); //opóźnienie
}
```

W powyższym przykładzie korzystamy z wyrażenia regularnego, które dopasowuje trzy słowa oddzielone spacjami. Możemy także zastosować różne znaki, aby tworzyć bardziej zaawansowane wzorce.

## Deep Dive

Aby lepiej zrozumieć i wykorzystywać wyrażenia regularne, warto poznać podstawowe elementy, z których się składają. Pierwszym z nich jest tzw. "metaznak", który jest specjalnym znakiem o innym znaczeniu niż jego literowe odpowiedniki. Przykłady takich znaków to "." (znak kropki, dopasowuje dowolny pojedynczy znak), "*" (dopasowuje 0 lub więcej wystąpień poprzedniego znaku) czy "+" (dopasowuje 1 lub więcej wystąpień poprzedniego znaku).

Kolejnym ważnym elementem są "klasy znaków", które pozwalają na dopasowanie konkretnego zestawu znaków. Przykładowo, [a-z] oznacza wszystkie litery od a do z, a [0-9] wszystkie cyfry od 0 do 9. Możemy także wykorzystać specjalne sekwencje, takie jak \d (dowolna cyfra), \w (dowolna litera lub cyfra) czy \s (dowolny biały znak).

Mając już podstawowe pojęcie o wyrażeniach regularnych, możemy korzystać z nich w swoich projektach Arduino, aby przetwarzać i analizować dane.

## Zobacz także

- [Dokumentacja biblioteki Regex](https://www.arduino.cc/reference/en/libraries/regex/)
- [Poradnik o wyrażeniach regularnych w Arduino](https://diyprojects.io/arduino-use-regular-expressions-replace-characters-example/)