---
title:                "Arduino: Pisanie testów"
programming_language: "Arduino"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/writing-tests.md"
---

{{< edit_this_page >}}

## Dlaczego pisać testy w programowaniu Arduino?

Chociaż często pisanie testów może wydawać się czasochłonne i zbędne, to jest to jednak ważna część procesu programowania. Testy pomagają wychwycić błędy w kodzie oraz zapewniają większą pewność, że nasz program będzie działał poprawnie. W przypadku programowania Arduino, gdzie nasz kod może wpływać na fizyczne elementy, takie jak diody LED czy silnik, jeszcze ważniejsze jest, aby posiadać odpowiednie testy.

## Jak pisać testy w Arduino?

Aby napisać testy w Arduino, musimy najpierw zainstalować bibliotekę Test na naszym komputerze. Wystarczy przejść do menadżera bibliotek w Arduino IDE i znaleźć bibliotekę Test. Następnie włączamy Test Suite Configuration i tworzymy nowy plik o nazwie test.ino.

```Arduino
#include <Test.h>

void setup(){
    Serial.begin(9600);
}

void loop(){
    //kod testów
    Test::run();
}
```

## Dogłębny wgląd w pisanie testów

Pozwólmy sobie teraz na trochę bardziej szczegółowy opis pisanie testów w Arduino. Po pierwsze, musimy zdefiniować wszystkie zmienne i funkcje, które chcemy przetestować. Możemy to zrobić, korzystając z metody `TEST()`. Musimy podać dwie wartości: nazwę testu oraz funkcję, którą chcemy przetestować.

```Arduino
TEST(nazwa_testu, funkcja_testowana){ 
    //kod testów
}
```

Wewnątrz funkcji definujemy warunki testu, jakie chcemy przetestować. Może to być sprawdzenie poprawności działania instrukcji warunkowych lub pętli, przypisanie wartości do zmiennych i sprawdzenie, czy zostały one poprawnie zapisane, lub po prostu sprawdzenie, czy nasz program działa zgodnie z oczekiwaniami. Do tego celu możemy użyć funkcji `ASSERT_*`, w zależności od tego, co chcemy sprawdzić.

```Arduino
TEST(nazwa_testu, funkcja_testowana){
    int x = 5;
    int y = 10;
    
    //sprawdzenie, czy x jest mniejsze niż y
    ASSERT_TRUE(x < y);
    
    //sprawdzenie, czy zmienna została poprawnie przypisana
    ASSERT_EQUALS(x, 5);
}
```

Możemy też wykorzystać funkcję `ASSERT_*` do weryfikacji, czy nasz program poprawnie komunikuje się z innymi urządzeniami, takimi jak czujniki czy ekrany. W ten sposób sprawdzimy nie tylko poprawność samych instrukcji, ale też komunikacji pomiędzy różnymi elementami.

## Zobacz także

- Oficjalna dokumentacja biblioteki Test dla Arduino - https://playground.arduino.cc/Code/UnitTest
- Podręcznik Test dla Arduino - https://create.arduino.cc/projecthub/radko9696/unipolar-stepper-with-arduino-b00815
- Przykładowy projekt wykorzystujący bibliotekę Test - https://github.com/adafruit/Adafruit_TCS34725/tree/master/examples/test