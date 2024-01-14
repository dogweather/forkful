---
title:                "Arduino: Tworzenie pliku tymczasowego"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Tworzenie tymczasowego pliku jest nieodłączną częścią programowania Arduino. Jest to przydatna umiejętność, ponieważ pozwala na tworzenie, przechowywanie i odczytywanie danych, które są potrzebne tylko w danym momencie wykonywania programu. W ten sposób można zaoszczędzić pamięć i zasoby mikrokontrolera.

## Jak to zrobić

Aby utworzyć tymczasowy plik w Arduino, należy wykonać kilka kroków:

1. Zacznij od zainicjowania zmiennej, która będzie przechowywać nazwę pliku. Na przykład ```String tempFile = "dane.txt";```
2. Następnie otwórz plik za pomocą funkcji ```File temp = SD.open(tempFile, FILE_WRITE);``` Co jest nazwą pliku i trybem działania (w tym przypadku zapis).
3. Możesz teraz pisać do pliku, używając funkcji ```temp.println("To jest przykładowy tekst.");```
4. Aby zamknąć plik, należy wywołać funkcję ```temp.close();```

Oto przykładowy kod, który tworzy tymczasowy plik i odczytuje z niego dane:

```Arduino
#include <SD.h> // Includujemy bibliotekę SD

String tempFile = "dane.txt"; // Inicjalizacja nazwy pliku

void setup() {
  Serial.begin(9600); // Inicjalizujemy komunikację szeregową
  SD.begin(8); // Inicjujemy moduł SD na pinie 8
  File temp = SD.open(tempFile, FILE_WRITE); // Tworzymy nowy plik o podanej nazwie i trybie zapisu
  temp.println("To jest przykładowy tekst."); // Zapisujemy dane do pliku
  temp.close(); // Zamykamy plik
}

void loop() {
  File temp = SD.open(tempFile); // Otwieramy plik w trybie odczytu
  while (temp.available()) { // Odczytujemy plik linia po linii, dopóki nie osiągniemy końca pliku
    String line = temp.readStringUntil('\n'); // Odczytujemy linię i zapisujemy ją w zmiennej
    Serial.println(line); // Wysyłamy linię przez port szeregowy
  }
  temp.close(); // Zamykamy plik
  delay(1000); // Odczekujemy 1 sekundę przed powtórzeniem pętli
}
```

Powyższy przykład wyświetli w konsoli szeregowej tekst "To jest przykładowy tekst." co sekundę. Proszę zauważyć, że można zmienić nazwę pliku i/lub tryb funkcji `SD.open()` w celu dostosowania go do własnych potrzeb.

## Deep Dive

Podczas tworzenia tymczasowego pliku w Arduino jest kilka rzeczy, które warto pamiętać:

- Nazwa pliku musi być unikatowa dla każdego zapisanego pliku. W przeciwnym razie funkcja `SD.open()` może zwrócić błąd lub nadpisać wcześniej utworzony plik o tej samej nazwie.
- Pamiętaj o zamknięciu pliku za pomocą funkcji `temp.close()`, aby zapobiec awarii systemu plików.
- Możesz również odczytać dane z pliku w trybie binarnym, a nie tekstowym, używając funkcji `File.read()` i `File.write()`.
- Więcej informacji na temat biblioteki SD można znaleźć na stronie [oficjalnej dokumentacji Arduino](https://www.arduino.cc/en/Reference/SD) lub korzystając z funkcji `help()` w Arduino IDE.

## Zobacz również

- [Oficjalna dokumentacja Arduino o tworzeniu plików](https://www.arduino.cc/en/Tutorial/Libra