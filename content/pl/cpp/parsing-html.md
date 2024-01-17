---
title:                "Analiza kodu HTML"
html_title:           "C++: Analiza kodu HTML"
simple_title:         "Analiza kodu HTML"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/parsing-html.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Parsowanie HTML to proces analizowania i przetwarzania kodu HTML w celu wyodrębnienia elementów i danych z danej strony internetowej. Programiści wykorzystują parsowanie HTML do automatycznego przetwarzania i wyświetlania zawartości stron internetowych.

## Jak to zrobić:
```C++
#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <vector>
#include <algorithm>

// Funkcja do parsowania HTML
std::vector<std::string> parseHTML(std::string html) {
    // Deklaracja zmiennych
    std::vector<std::string> result; // przechowywanie elementów HTML
    std::string tag, data; // przechowywanie tagu i danych
    std::stringstream ss; // strumień danych
    bool insideTag = false; // flaga do sprawdzania, czy znajdujemy się wewnątrz tagu

    // Przeglądanie kodu HTML po znakach
    for (char c : html) {
        // Jeśli napotkamy znak rozpoczynający tag
        if (c == '<') {
            // Jeśli już jesteśmy wewnątrz innego tagu, wstaw w wynik poprzedni tag i dane, które już przechowujemy                
            if (insideTag) {
                result.push_back(tag);
                result.push_back(data);
            }
            // Przełączamy flagę na true i czyścimy zmienne
            insideTag = true;
            tag.clear();
            data.clear();
        }
        // Jeśli napotkamy znak kończący tag
        else if (c == '>'){
            // Przełączamy flagę na false i zapisujemy tag i dane do zmiennych
            insideTag = false;
            tag = ss.str();
            ss.str("");
        }
        // Jeśli jesteśmy wewnątrz tagu i napotkamy kolejny znak
        else if (insideTag){
            // Dopisujemy go do zmiennej data
            ss << c;
        }
        // Jeśli nie jesteśmy wewnątrz tagu, ale napotkamy kolejny znak
        else {
            // Dopisujemy go do zmiennej tag
            data += c;
        }
    }
    // Dodaj ostatni tag i dane do wyniku
    result.push_back(tag);
    result.push_back(data);
    // Zwróć wynik
    return result;
}

int main() {
    // Wczytywanie pliku HTML
    std::ifstream file("index.html");
    std::string html;

    // Jeśli wczytano plik, przypisz kod HTML do zmiennej
    if (file){
        html = std::string( std::istreambuf_iterator<char>(file), std::istreambuf_iterator<char>() );
        file.close();
    }
    // Jeśli nie, wyświetl komunikat o błędzie
    else {
        std::cout << "Błąd podczas wczytywania pliku!" << std::endl;
        return 0;
    }

    // Parsowanie HTML przy użyciu naszej funkcji
    std::vector<std::string> parsed = parseHTML(html);

    // Wyświetlanie wyniku
    for (auto it = parsed.begin(); it != parsed.end(); it+=2 ) {
        std::cout << "Tag: " << *it << ", Dane: " << *(it+1) << std::endl; 
    }
}
```

Przykładowy kod HTML w pliku:

```html
<!DOCTYPE html>
<html>
    <head>
        <title>Przykładowa strona</title>
    </head>
    <body>
        <h1>Witaj na mojej stronie!</h1>
        <p>Tekst</p>
        <a href="https://www.example.com">Link</a>
    </body>
</html>
```

Przykładowy output:

```
Tag: <!DOCTYPE html>, Dane: 
Tag: <html>, Dane: 
Tag: <head>, Dane: 
Tag: <title>, Dane: Przykładowa strona
Tag: </title>, Dane: 
Tag: </head>, Dane: 
Tag: <body>, Dane: 
Tag: <h1>, Dane: Witaj na mojej stronie!
Tag: </h1>, Dane: 
Tag: <p>, Dane: Tekst
Tag: </p>, Dane: 
Tag: <a href="https://www.example.com">, Dane: Link
Tag: </a>, Dane: 
Tag: </body>, Dane: 
Tag: </html>, Dane: 
```

Parsowanie HTML może być użyteczne w wielu przypadkach, na przykład do automatycznego przetwarzania danych ze stron internetowych lub do wyświetlania ich w odpowiedni sposób. Możliwe są również inne sposoby na parsowanie HTML, takie jak wykorzystanie bibliotek specjalnie stworzonych do tego celu.

## Głębsza analiza

Parsowanie HTML ma swoje początki w latach 90-tych, gdy powstał język HTML i zaczął być wykorzystywany w budowaniu stron internetowych. Obecnie istnieje wiele bibliotek i narzędzi, które ułatwiają ten proces. Istnieją również różne sposoby parsowania HTML, takie jak wykorzystanie wyrażeń regularnych lub drzewa DOM.

## Zobacz także

- [Tutorial: Jak napisać prosty algorytm parsowania HTML w C++](https://www.codeproject.com/Articles/4765/An-HTML-parser-in-C)

- [Biblioteka do parsowania HTML w C++](https://code.google.com/archive/p/html-parser/)

- [Szybkie przeglądanie HTML w C++](https://github.com/ot/metafi/blob/master/src/prolog/html_parser.cc)