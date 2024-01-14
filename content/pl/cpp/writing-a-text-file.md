---
title:                "C++: Tworzenie pliku tekstowego"
simple_title:         "Tworzenie pliku tekstowego"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie plików tekstowych jest jednym z podstawowych zadań w programowaniu. Jest to niezbędnym krokiem w procesie tworzenia oprogramowania, ponieważ pozwala na zapisanie i przechowywanie danych w formacie tekstowym. Pliki tekstowe są wykorzystywane w wielu programach, takich jak edytory tekstu, przeglądarki internetowe czy bazy danych. Dlatego też każdy programista powinien umieć pisać pliki tekstowe w swoim języku programowania.

## Jak to zrobić

Pisanie plików tekstowych w języku C++ jest proste i wymaga wykorzystania kilku podstawowych funkcji. Umożliwiają one otwarcie pliku, zapisanie danych, a następnie zamknięcie pliku. Poniżej przedstawiamy przykładowy kod w języku C++, który pokazuje, jak napisać plik tekstowy:

```C++
#include <iostream>
#include <fstream>

using namespace std;

int main()
{
    // otwieramy plik w trybie zapisu
    ofstream plik("tekst.txt", ios::out);

    // sprawdzamy, czy plik został otwarty poprawnie
    if(plik.good())
    {
        // zapisujemy tekst do pliku
        plik << "To jest przykładowy tekst do zapisania w pliku." << endl;
        
        // zamykamy plik
        plik.close();

        // wyświetlamy komunikat o poprawnym zapisaniu pliku
        cout << "Plik tekst.txt został pomyślnie zapisany." << endl;
    }
    else
    {
        // wyświetlamy komunikat o błędzie otwarcia pliku
        cout << "Wystąpił błąd podczas otwierania pliku." << endl;
    }
    
    return 0;
}
```

Po uruchomieniu powyższego kodu, w katalogu z projektem powinien pojawić się plik o nazwie "tekst.txt", który zawierać będzie tekst: "To jest przykładowy tekst do zapisania w pliku."

## Dogłębna analiza

Pisanie plików tekstowych w języku C++ wymaga od nas kilku rzeczy. Po pierwsze, musimy wykorzystać nagłówek <fstream>, który pozwala na operacje na plikach. Następnie za pomocą funkcji ofstream otwieramy plik w trybie zapisu. Możemy także wybrać inny tryb, takich jak odczyt albo tryb binarny. Po otwarciu pliku, możemy zacząć zapisywać do niego dane za pomocą operatora strumieniowego, a następnie zamknąć plik wywołując funkcję close().

Warto także pamiętać, że przed przystąpieniem do operacji na plikach, powinniśmy sprawdzić, czy plik został otwarty poprawnie. Można to zrobić przy użyciu funkcji good(), która zwraca true, jeśli operacje na pliku są możliwe.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o operacjach na plikach w języku C++, zapoznaj się z poniższymi linkami:

- [Dokumentacja języka C++ na temat operacji na plikach](http://www.cplusplus.com/doc/tutorial/files/)
- [Przykłady kodu na GitHubie](https://github.com/search?q=c%2B%2B+file+operations&type=Repositories)