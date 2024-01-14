---
title:    "Arduino: Pisanie pliku tekstowego"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Kodowanie jest nie tylko wspaniałym hobby, ale także użytecznym narzędziem do rozwiązywania codziennych problemów. Dzięki Arduino możesz tworzyć różnorodne projekty, a pisanie tekstowych plików może być bardzo przydatne w tym procesie. Poniżej dowiesz się, dlaczego warto nauczyć się tego prostego zadania.

## Jak to zrobić

Aby zapisać tekstowy plik za pomocą Arduino, wykonaj następujące kroki:

1. Uruchom środowisko programistyczne Arduino IDE i otwórz projekt.
2. Wybierz przykładowy kod poniżej i wklej go do swojego projektu:

```Arduino
#include <SPI.h> //biblioteka do komunikacji z kartą SD
#include <SD.h>  //biblioteka pozwalająca na zapisywanie i odczytywanie danych z karty SD

void setup() {
  Serial.begin(9600); //inicjalizacja komunikacji szeregowej
  while(!Serial); //czekaj na połączenie
  Serial.print("Tworzenie nowego pliku...");
  if (SD.begin(10)) { //sprawdzenie czy karta SD jest dostępna
    Serial.println("Karta SD gotowa do użycia");
    File dataFile = SD.open("dane.txt", FILE_WRITE); //utwórz plik o nazwie "dane.txt" i otwórz go do zapisu
    if (dataFile) {
      dataFile.println("To jest przykładowy tekst do zapisania w pliku.");
      dataFile.close(); //zamknij plik
      Serial.println("Plik został zapisany.");
    }
  }
  else { //jeśli nie udało się uzyskać dostępu do karty SD
    Serial.println("Błąd karty SD.");
  }
}

void loop() {
  //puste
}
```

3. Włóż kartę SD do czytnika i podłącz go do swojego Arduino.
4. Skompiluj i wgraj kod na płytkę. Jeśli wszystko przebiegnie pomyślnie, na wyjściu szeregowym zobaczysz komunikat "Plik został zapisany".
5. Podłącz kabel USB z Arduino do komputera i otwórz program do przeglądania plików. W folderze "arduino" powinien pojawić się plik "dane.txt".

## Głębsze spojrzenie

W powyższym przykładzie użyto biblioteki SD.h, która pozwala na komunikację z kartą SD. Aby skutecznie zapisać plik tekstowy, konieczne jest wykonanie kilku kroków:

- Sprawdzenie dostępności karty SD i jej inicjalizacja.
- Tworzenie nowego pliku o wybranej nazwie.
- Otwarcie pliku do zapisu, w którym można wpisać wybrany tekst.
- Zamknięcie pliku.

Warto również zauważyć, że istnieje możliwość odczytu danych z pliku tekstowego, co daje dużo większe możliwości przy tworzeniu projektów z wykorzystaniem Arduino.

## Zobacz również

1. [Tutorial na temat wprowadzenia do Arduino](https://pcm.pl/view_page.php?page_id=273)
2. [Oficjalna strona Arduino](https://www.arduino.cc/)
3. [Biblioteka SD.h](https://www.arduino.cc/en/reference/SD)
4. [Inne przydatne biblioteki do komunikacji z urządzeniami peryferyjnymi](https://playground.arduino.cc/Main/LibraryList/)