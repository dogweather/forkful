---
title:                "Pisanie pliku tekstowego"
html_title:           "C++: Pisanie pliku tekstowego"
simple_title:         "Pisanie pliku tekstowego"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie plików tekstowych jest ważnym elementem programowania w C++. Pozwala ono na przechowywanie i przetwarzanie danych w sposób uporządkowany oraz umożliwia komunikację między programem a użytkownikami.

## Jak to zrobić

```C++
#include <iostream>
#include <fstream>

using namespace std;

int main() {
  // Stwórz obiekt pliku tekstowego
  fstream plik;

  // Otwórz plik do zapisu
  plik.open("plik.txt", ios::out);

  // Sprawdź, czy plik został poprawnie otwarty
  if (!plik) {
    cout << "Błąd otwarcia pliku!" << endl;
    return 1;
  }

  // Zapisz dane do pliku
  plik << "To jest przykładowy tekst." << endl;
  plik << "Ta linia zostanie dodana do pliku." << endl;

  // Zamknij plik
  plik.close();

  // Otwórz plik do odczytu
  plik.open("plik.txt", ios::in);

  // Sprawdź, czy plik został poprawnie otwarty
  if (!plik) {
    cout << "Błąd otwarcia pliku!" << endl;
    return 1;
  }

  // Odczytaj zawartość pliku i wyświetl ją na ekranie
  string linia;
  while (getline(plik, linia)) {
    cout << linia << endl;
  }

  // Zamknij plik
  plik.close();

  return 0;
}
```

Przedstawiony powyżej kod pokazuje, jak otworzyć plik do zapisu, zapisać do niego dane, a następnie otworzyć ten sam plik do odczytu i wyświetlić jego zawartość na ekranie. W linii 6 używamy obiektu `fstream` do utworzenia pliku tekstowego. W linii 9 otwieramy plik do zapisu, a w linii 22 otwieramy go do odczytu. W linii 13 i 14 zapisujemy dane do pliku, a w linii 26 i 27 odczytujemy je z pliku.

## Głęboki zanurzenie

Pisanie plików tekstowych jest ważną częścią programowania w C++. Oprócz przechowywania i przetwarzania danych, pliki tekstowe mogą być również użyte do komunikacji z innymi programami lub użytkownikami. Aby bardziej zaawansowane operacje na plikach, takie jak dodawanie, usuwanie lub przeszukiwanie danych, warto zapoznać się z biblioteką standardową C++ - `<fstream>`, ponieważ dostarcza ona większą ilość funkcji do pracy z plikami.

## Zobacz również

1. Rozmowa z programistą: 10 powodów, dlaczego warto nauczyć się programowania w C++ (https://codecouple.pl/rozmowa-z-programista/10-powodow-dlaczego-warto-nauczyc-sie-programowania-w-c/)
2. Oficjalna dokumentacja biblioteki `fstream` (https://en.cppreference.com/w/cpp/io/basic_fstream)