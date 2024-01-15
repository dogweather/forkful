---
title:                "Zamiana tekstu na wielkie litery"
html_title:           "C++: Zamiana tekstu na wielkie litery"
simple_title:         "Zamiana tekstu na wielkie litery"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Napisanie programu może czasami wymagać zmiany wielkości liter w tekście. Może to być na przykład potrzebne do wyświetlenia nazwy użytkownika w kapitalikach lub do poprawnego sformatowania danych. W tej krótkiej instrukcji dowiesz się, jak w prosty sposób zaimplementować funkcję, która automatycznie zamieni litery w ciągu znaków na wielkie.

## Jak to zrobić

Zacznijmy od zadeklarowania funkcji, która będzie odpowiedzialna za zmianę wielkości liter w naszym tekście.

```C++
void capitalize(string& str) {
    // kod zmieniający wielkość liter w ciągu znaków
}
```

Następnie, musimy przeiterować przez cały ciąg znaków i zmienić wielkość liter za pomocą wbudowanej funkcji `toupper()`. Pamiętaj również o sprawdzeniu, czy ciąg jest już w całości zapisany wielkimi literami, aby uniknąć niepotrzebnych zmian.

```C++
void capitalize(string& str) {
    for(char& c : str) {
        c = toupper(c);
    }
}
```

Aby przetestować naszą funkcję, możemy stworzyć prosty program, który przyjmie od użytkownika tekst i wyświetli go w kapitalikach.

```C++
int main() {
    string text;
    cout << "Podaj tekst: ";
    getline(cin, text);

    capitalize(text);
    cout << "Tekst z wielkimi literami: " << text << endl;

    return 0;
}
```

Po uruchomieniu programu i wpisaniu tekstu, zostanie on wyświetlony z wielkimi literami.

```
Podaj tekst: C++ to fajny język programowania
Tekst z wielkimi literami: C++ TO FAJNY JĘZYK PROGRAMOWANIA
```

## Deep Dive

W przypadku, gdy potrzebujemy zmienić wielkość liter jedynie wybranych słów, możemy skorzystać z funkcji `isalpha()` do sprawdzenia, czy dany znak jest literą, co pozwoli nam na precyzyjniejszą kontrolę nad zmianami.

```C++
void capitalize(string& str) {
    bool capitalizeNext = true;
    for(char& c : str) {
        if(isalpha(c)) {
            if(capitalizeNext) {
                c = toupper(c);
                capitalizeNext = false;
            } else {
                c = tolower(c);
            }
        } else {
            capitalizeNext = true;
        }
    }
}
```

W przypadku tekstu "C++ to fajny język programowania", funkcja wyprodukuje wynik "C++ To Fajny Język Programowania". Zauważ, że kwota "C++" pozostaje niezmieniona, a pozostałe słowa zostają wyświetlone z jedynie pierwszą literą jako wielką.

## Zobacz również

- [Funkcja toupper() w C++](https://www.programiz.com/cpp-programming/library-function/cctype/toupper)
- [Poradnik o operacjach na ciągach znaków w C++](https://www.tutorialspoint.com/cplusplus/cpp_strings.htm)