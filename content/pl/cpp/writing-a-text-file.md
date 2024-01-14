---
title:                "C++: Tworzenie pliku tekstowego"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie plików tekstowych może wydawać się banalnym zadaniem, ale faktycznie jest to bardzo przydatna umiejętność w programowaniu. Pliki tekstowe pozwalają na przechowywanie danych w formacie tekstowym, który jest łatwo dostępny i czytelny dla ludzi oraz różnych programów. Dzięki temu, możliwe jest przechowywanie i przetwarzanie dużych ilości danych w wygodnej i zorganizowanej formie.

## Jak to zrobić

Aby napisać plik tekstowy w języku C++, wystarczy wykorzystać klasę `ofstream` oraz jej metody. Najpierw należy utworzyć obiekt tej klasy, wskazując przy tym nazwę nowego pliku oraz tryb otwarcia. Następnie, wykorzystując metodę `open()`, otwieramy plik do zapisu. W szczególności, możemy wykorzystać tryb `ios::app`, który pozwala na dopisywanie danych do istniejącego pliku. W celu zapisania danych, należy wykorzystać metodę `write()`, która przyjmuje jako parametry wskaźnik na bufor danych oraz ilość bajtów do zapisania. Po zakończeniu zapisywania, należy pamiętać o zamknięciu pliku za pomocą metody `close()`.

Przykładowy kod wykorzystujący te metody wyglądałby następująco:

```C++
#include <iostream>
#include <fstream>

int main() {
    // utworzenie i otwarcie pliku do zapisu
    std::ofstream file("example.txt");
    // sprawdzenie, czy plik został poprawnie otwarty
    if (!file.is_open()) {
        // obsługa błędu
        std::cout << "Błąd: nie udało się otworzyć pliku do zapisu!" << std::endl;
        return 1;
    }
    // zapisanie tekstu do bufora
    const char* text = "Przykładowy tekst do zapisania.";
    // wyznaczenie długości tekstu
    int length = strlen(text);
    // zapisanie tekstu do pliku
    file.write(text, length);
    // zamknięcie pliku
    file.close();
    return 0;
}
```

Po uruchomieniu powyższego kodu, plik `example.txt` powinien zostać utworzony w katalogu, w którym znajduje się nasz program. Jeśli plik już istnieje, to zostanie on nadpisany, jeśli chcemy dopisać dane do istniejącego pliku, należy wykorzystać tryb `app` zamiast `out` przy otwieraniu.

## Głębsze wyjaśnienie

Podczas pisania plików tekstowych, ważne jest również dbanie o poprawne kodowanie znaków. W języku C++, najczęściej wykorzystywana jest tzw. tabela ASCII, która określa, jaki kod odpowiada danemu znakowi. Dlatego w powyższym przykładzie, tekst został przekonwertowany na kod ASCII przy użyciu funkcji `strlen()`. W celu uniknięcia błędów, zawsze należy upewnić się, że używane funkcje i struktury danych są zgodne z odpowiednim kodowaniem znaków.

## Zobacz także

- [Klasa `ofstream` w języku C++](https://www.geeksforgeeks.org/ofstream-class-in-cpp/)
- [Poradnik o plikach tekstowych w języku C++](https://www.tutorialspoint.com/cplusplus/cpp_files_streams.htm)
- [Otwieranie pliku w trybie `app` w języku C++](https://www.cplusplus.com/reference/fstream/ofstream/open/)