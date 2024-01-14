---
title:    "C++: Łączenie ciągów znaków"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Dlaczego

Konkatenacja (łączenie) łańcuchów znakowych jest niezbędnym narzędziem w języku C++, które pozwala na tworzenie bardziej kompleksowych i użytecznych programów. W artykule tym omówimy, dlaczego warto nauczyć się tego podstawowego elementu programowania.

## Jak to zrobić

Konkatenacja łańcuchów znakowych jest stosunkowo prostym procesem w języku C++. W celu połączenia dwóch lub więcej łańcuchów używamy operatora "+" lub funkcji "concat". Przykładowy kod wykorzystujący oba sposoby wyglądałby następująco:

```C++
#include <iostream>
using namespace std;

int main() {
    string str1 = "Witaj";
    string str2 = "świecie";
    string str3 = str1 + str2; // operator "+"
    string str4 = str1.concat(str2); // funkcja "concat"
    
    cout << str3 << endl;
    cout << str4 << endl;
    return 0;
}
```

Kod ten wypisze na ekranie: "Witajświecie" oraz "Witajświecie", ponieważ operator "+" oraz funkcja "concat" wykonują dokładnie to samo zadanie - łączą przekazane do nich łańcuchy znakowe w jeden. Warto również zauważyć, że dla operatora "+" zawsze trzeba konwertować liczby na łańcuchy, podczas gdy funkcja "concat" to automatycznie robi.

## Głębsze zagadnienia

W języku C++ można także łączyć łańcuchy znakowe z innymi zmiennymi, np. liczbami. Jednak w takim wypadku wymagane jest użycie funkcji "to_string", która konwertuje różne typy danych na łańcuchy znakowe. Przykładowy kod wykorzystujący to podejście:

```C++
#include <iostream>
using namespace std;

int main() {
    int wiek = 25;
    string msg1 = "Mam " + to_string(wiek) + " lat.";
    string msg2 = "";
    cout << "Podaj imię: ";
    cin >> msg2; // wczytanie wartości od użytkownika
    string msg3 = "A nazywam się " + msg2 + ".";
    
    cout << msg1 << endl;
    cout << msg3 << endl;
    return 0;
}
```

Kod ten wyświetli na ekranie: "Mam 25 lat." oraz "A nazywam się [imię podane przez użytkownika]." Dzięki funkcji "to_string" możemy dowolnie łączyć łańcuchy znakowe i zmienne, co niesamowicie rozszerza możliwości programowania.

## Zobacz też

- [Dokumentacja C++ na temat łańcuchów znakowych](https://pl.wikibooks.org/wiki/C%2B%2B/Nauka/Łańcuchy_znaków)
- [Tutorial na temat konkatenacji łańcuchów w C++](https://www.programiz.com/cpp-programming/string-concatenation)
- [Słownik terminów związanych z programowaniem w języku C++](https://www.46578.pl/programowanie.html)