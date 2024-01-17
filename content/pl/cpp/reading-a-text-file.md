---
title:                "Odczytywanie pliku tekstowego"
html_title:           "C++: Odczytywanie pliku tekstowego"
simple_title:         "Odczytywanie pliku tekstowego"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Odczyt pliku tekstowego jest procesem, w którym programista wczytuje zawartość pliku tekstowego do swojego programu. Programiści robią to w celu uzyskania dostępu do danych przechowywanych w pliku i wykorzystania ich w swoim kodzie.

## Jak to zrobić:

```C++
#include <iostream>
#include <fstream>

using namespace std;

int main() {

    // Otwieranie pliku do odczytu
    ifstream plik("tekst.txt");

    // Sprawdzanie czy plik istnieje
    if (!plik) {
        // Obsługa błędu jeśli plik nie istnieje
        cout << "Nie udalo sie otworzyc pliku!";
        return 1;
    }

    // Wczytywanie danych do zmiennej typu string
    string linijka;
    getline(plik, linijka);

    // Wypisanie zawartości pliku na ekranie
    cout << linijka;

    // Zamykanie pliku
    plik.close();

    return 0;
}
```

**Wynik:**
"Przykładowy tekst w pliku."

## Deep Dive:

Historia odczytu pliku tekstowego sięga początków programowania. Wcześniej, gdy programy były wykonywane na kartach perforowanych, programiści wczytywali dane do swoich programów, wkładając karty do czytników kart. Obecnie, odczyt pliku tekstowego jest jedną z najczęstszych operacji wykonywanych przez programistów, ponieważ pozwala im na pracę z różnymi typami danych przechowywanymi w pliku.

Alternatywą dla odczytu pliku tekstowego może być odczyt pliku binarnego, w którym dane są zapisane w postaci binarnej zamiast tekstowej. Odczyt plików tekstowych jest bardziej czytelny i łatwiejszy w implementacji, dlatego jest częściej stosowany.

Implementacja odczytu pliku tekstowego w języku C++ jest możliwa dzięki zestawowi bibliotek standardowych takich jak ```<fstream>``` i ```<iostream>```, które dostarczają funkcje i metody do wczytywania oraz zamykania pliku.

## Zobacz też:

Dla głębszego zrozumienia odczytywania plików tekstowych w języku C++, warto zapoznać się z dokumentacją biblioteki standardowej ```<fstream>```. Link do dokumentacji: https://en.cppreference.com/w/cpp/header/fstream