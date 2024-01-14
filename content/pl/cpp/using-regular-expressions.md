---
title:    "C++: Korzystanie z wyrażeń regularnych"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Dlaczego

Regular expressions są jednym z najważniejszych narzędzi w języku C++, ponieważ pozwalają na efektywne przetwarzanie i manipulację tekstem. Używanie wyrażeń regularnych ułatwia tworzenie skomplikowanych wzorców i przeprowadzanie precyzyjnych wyszukiwań, co jest niezwykle przydatne w wielu projektach programistycznych.

## Jak to zrobić

Aby korzystać z wyrażeń regularnych w C++, musimy najpierw zaimportować odpowiednią bibliotekę - ```<regex>```. Następnie możemy używać funkcji i klas z tej biblioteki do wykonywania różnych operacji na łańcuchach znaków.

Przykładowo, jeśli chcielibyśmy znaleźć wszystkie liczby całkowite w danym tekście, możemy użyć konstrukcji ```regex_search```, która przeszukiwać będzie dany tekst pod kątem odpowiedniego wzorca. Poniżej znajduje się kod, który wyświetla wszystkie znalezione liczby całkowite i ich pozycje w tekście:
```C++
#include <iostream>
#include <regex>
using namespace std;

int main() {
    string text = "Witaj 123 na świecie 456!";
    regex pattern("\\d+"); // szukaj ciągu składającego się z jednej lub więcej cyfr
    smatch matches; // przechowuje informacje o dopasowaniach
    while(regex_search(text, matches, pattern)) {
        cout << "Znaleziono " << matches.str() << " od indeksu " << matches.position() << endl;
        text = matches.suffix().str(); // tekst począwszy od końca ostatniego dopasowania
    }
    return 0;
}
```

W powyższym przykładzie użyliśmy wyrażenia regularnego ```\\d+```, aby znaleźć wszystkie ciągi składające się z jednej lub więcej cyfr. Zauważmy również, że funkcja ```regex_search``` zwraca informację o dopasowaniu w postaci obiektu ```smatch```, który zawiera m.in. metodę ```str()``` pozwalającą na pobranie dopasowanego tekstu oraz metodę ```position()``` zwracającą pozycję dopasowania w tekście.

Powyższy kod wyświetli następujące wyniki:
```
Znaleziono 123 od indeksu 6
Znaleziono 456 od indeksu 19
```

## Głębszy zanurzenie

Dużą zaletą wyrażeń regularnych jest możliwość tworzenia skomplikowanych wzorców, które pozwalają na precyzyjne wyszukiwanie i manipulację tekstem. Oprócz podstawowej składni, w której używamy znaków specjalnych do tworzenia wzorców, istnieje również możliwość korzystania z klasy ```regex```, która pozwala na tworzenie bardziej zaawansowanych wyrażeń regularnych.

Korzystanie z wyrażeń regularnych może być nieco skomplikowane dla początkujących programistów, ale po pewnym czasie nauki stanie się niezwykle przydatnym narzędziem w codziennej pracy. Warto poświęcić trochę czasu na zgłębienie tematu i zapoznanie się z różnymi możliwościami, jakie oferują wyrażenia regularne w języku C++.

## Zobacz też

- [Dokumentacja biblioteki <regex>](https://www.cplusplus.com/reference/regex/)
- [Wyrażenia regularne w C++ dla początkujących](https://www.cprogramming.com/tutorial/regular-expressions-c++.html)