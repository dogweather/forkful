---
title:                "Arduino: Odczytywanie pliku tekstowego"
simple_title:         "Odczytywanie pliku tekstowego"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Dlaczego warto czytać pliki tekstowe

Czytanie plików tekstowych jest ważnym elementem programowania Arduino, ponieważ pozwala na dostęp do różnego rodzaju danych przechowywanych w plikach. Może to obejmować ustawienia, dane wejściowe lub instrukcje. Dzięki temu możesz wykorzystać te informacje do sterowania swoim projektem lub wyświetlenia ich na ekranie. W tym wpisie dowiesz się, jak w prosty sposób odczytać plik tekstowy za pomocą Arduino.

# Jak to zrobić

Aby odczytać plik tekstowy, potrzebujesz modułu SD lub pamięci flash. Najpierw musisz utworzyć obiekt do obsługi modułu SD lub pamięci flash:

```
Arduino SD card;
```

Następnie musisz otworzyć plik za pomocą funkcji "open ()" i przekazać mu nazwę pliku oraz tryb dostępu:

```
File file = SD.open ("plik.txt", FILE_READ);
```

Teraz możesz użyć funkcji "readString ()", aby odczytać cały tekst z pliku i przypisać go do zmiennej tekstowej:

```
String data = file.readString();
```

Warto pamiętać, że wartość odczytana z pliku jest typu String, więc jeśli potrzebujesz jej jako liczby, musisz użyć funkcji "toInt ()" lub "toFloat ()".

```
int liczba = data.toInt();
float wartosc = data.toFloat();
```

Na koniec nie zapomnij zamknąć pliku za pomocą funkcji "close ()":

```
file.close();
```

# Wnikliwa analiza

Oprócz funkcji "readString ()", Arduino oferuje także inne metody odczytywania plików tekstowych, takie jak "readInt ()", "readFloat ()" lub "readBytes ()". Funkcje te można wykorzystać do precyzyjnego odczytywania danych w odpowiednim formacie. W przypadku dużej ilości danych, zaleca się wykorzystanie pętli for, aby wczytać jeden znak na raz.

Ponadto, jeśli potrzebujesz odczytać plik z innego źródła niż moduł SD lub pamięć flash, możesz skorzystać z funkcji "WiFiClient" lub "HttpClient", jeśli korzystasz z sieci Wi-Fi.

# Zobacz również

Jeśli chcesz dowiedzieć się więcej o odczytywaniu plików tekstowych za pomocą Arduino, możesz zapoznać się z dokumentacją oficjalnego projektu Arduino lub poszukać tutoriali i przykładów online. Poniżej znajdują się linki do kilku przydatnych źródeł:

- Oficjalna dokumentacja: https://www.arduino.cc/en/Reference/FileReadString
- Przykład odczytywania pliku z modułu SD: https://www.arduino.cc/en/Tutorial/ReadASCIITable
- Poradnik odczytywania pliku z pamięci flash: https://randomnerdtutorials.com/esp32-data-logging-temperature-to-microsd-card/
- Dokumentacja o wykorzystaniu funkcji "WiFiClient": https://www.arduino.cc/en/Reference/WiFiClient
- Przykład wczytywania danych z sieci za pomocą "HttpClient": https://www.programmingelectronics.com/using-httpclient-to-pull-data-from-the-web/