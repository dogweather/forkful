---
title:    "C++: Usuwanie znaków odpowiadających wzorcowi"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Dlaczego warto usuwać znaki pasujące do wzorca?

Czy zdarzyło Ci się kiedyś napisać program, który musiałby przetwarzać duże ilości tekstowych danych? Jeśli tak, prawdopodobnie zdajesz sobie sprawę, jak ważne jest wydajne zarządzanie i przetwarzanie tego tekstu. Często zdarza się, że musimy usunąć pewne znaki, które są zbędne lub niepożądane dla naszego programu. W takiej sytuacji, warto poznać narzędzia, które pomogą nam w tym zadaniu.

# Jak to zrobić?

Jednym ze sposobów na usuwanie znaków pasujących do wzorca jest użycie metody `erase` z biblioteki `algorithm` w języku C++. Poniżej przedstawiam przykładowy kod, który usuwa wszystkie spacje z tekstu:

```C++
#include <iostream>
#include <algorithm>
#include <string>

int main() {

    std::string tekst = "To jest tekst z niepotrzebnymi        spacjami.";
    
    // użycie metody erase z biblioteki algorithm
    tekst.erase(std::remove(tekst.begin(), tekst.end(), ' '), tekst.end());
    
    std::cout << tekst;

    return 0;
}
```

Po uruchomieniu tego kodu, wyjściem będzie: `Tojesttekstzniepotrzebnymispacjami.` Warto jednak zauważyć, że metoda ta usuwa nie tylko spacje, ale również wszystkie inne podane przez nas znaki.

# Głębszy zanurzenie

Aby bardziej zrozumieć działanie metody `erase`, warto przyjrzeć się jej definicji. Metoda ta przyjmuje dwa argumenty - iterator na początek i koniec fragmentu, który chcemy usunąć. W powyższym przykładzie użyliśmy metody `remove`, która jako pierwszy argument przyjmuje iterator na początek naszego tekstu, a jako drugi - iterator na jego koniec. Następnie, metoda `erase` usuwa wszystkie elementy pomiędzy tymi iteratorami.

Ciekawą alternatywą dla tej metody jest użycie `regex`, czyli wyrażeń regularnych, które pozwalają nam znacznie bardziej precyzyjnie wybierać znaki do usunięcia. Poniżej przedstawiam przykładowy kod wykorzystujący wyrażenia regularne do usunięcia wszystkich cyfr z tekstu:

```C++
#include <iostream>
#include <regex>

int main() {

    std::string tekst = "123To 4jest 56tekst 789z cyframi000.";
    
    // stworzenie wyrażenia regularnego dopasowującego cyfry
    std::regex cyfry("\\d");
    
    // użycie metody regex_replace z biblioteki regex
    // drugi argument określa znaki zastępujące cyfry
    tekst = std::regex_replace(tekst, cyfry, "");
    
    std::cout << tekst;

    return 0;
}
```

Wyjściem z tego kodu będzie: `To jest tekst z cyframi.` Ważne jest jednak, aby zwrócić uwagę na wydajność tego rozwiązania - wyrażenia regularne mogą być bardzo uciążliwe dla procesora, więc w przypadku dużej liczby danych lepiej jest skorzystać z innego narzędzia.

# Zobacz także

- [Dokumentacja biblioteki algorithm - metoda erase](https://cppreference.com/w/cpp/algorithm/erase)
- [Dokumentacja biblioteki regex - metoda regex_replace](https://cppreference.com/w/cpp/regex/regex_replace)
- [Poradnik wideo o usuwaniu znaków z tekstu w języku C++](https://www.youtube.com/watch?v=tOyy6OuqlB0)