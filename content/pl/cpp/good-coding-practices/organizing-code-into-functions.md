---
date: 2024-01-26 01:10:04.986266-07:00
description: "Podzia\u0142 kodu na funkcje oznacza, \u017Ce rozdzielamy nasz kod na\
  \ mniejsze, wielokrotnego u\u017Cytku cz\u0119\u015Bci. Robimy to, aby unika\u0107\
  \ powt\xF3rze\u0144, uczyni\u0107 nasz kod\u2026"
lastmod: '2024-03-13T22:44:35.718437-06:00'
model: gpt-4-1106-preview
summary: "Podzia\u0142 kodu na funkcje oznacza, \u017Ce rozdzielamy nasz kod na mniejsze,\
  \ wielokrotnego u\u017Cytku cz\u0119\u015Bci. Robimy to, aby unika\u0107 powt\xF3\
  rze\u0144, uczyni\u0107 nasz kod\u2026"
title: Organizacja kodu w funkcje
weight: 18
---

## Co i dlaczego?
Podział kodu na funkcje oznacza, że rozdzielamy nasz kod na mniejsze, wielokrotnego użytku części. Robimy to, aby unikać powtórzeń, uczynić nasz kod czytelnym oraz uprościć debugowanie i testowanie. Dobrze zorganizowane funkcje mogą być jak posiadanie pudełka starannie oznakowanych narzędzi, gotowych do użytku i podzielenia się nimi.

## Jak to zrobić:
Weźmy za przykład wspólne zadanie: obliczanie pola koła. Zamiast pisać ten sam wzór za każdym razem, umieszczamy go w funkcji.

```C++
#include <iostream>
#define PI 3.14159

double calculateCircleArea(double radius) {
    return PI * radius * radius;
}

int main() {
    double r = 5.0;
    std::cout << "Pole koła o promieniu " << r << " wynosi " << calculateCircleArea(r) << std::endl;
    return 0;
}
```

Przykładowe wyjście:
```
Pole koła o promieniu 5 wynosi 78.5397
```

## Szczegółowa analiza
Historycznie procedury i funkcje stanowiły kręgosłup programowania strukturalnego, promowanego w latach 60. w celu zwalczania problemów z "spaghetti code" we wcześniejszych językach programowania imperatywnego. Alternatywy takie jak OOP (Programowanie Obiektowe) poszły dalej, kojarząc te funkcje ze strukturami danych. W C++ mamy do czynienia z regularnymi funkcjami, metodami klas (w tym metodami statycznymi), lambdami i funkcjami szablonowymi, z których każda oferuje różne korzyści. Implementacja dobrze zorganizowanych funkcji zwykle wiąże się z przestrzeganiem zasad takich jak DRY ("Don't Repeat Yourself" - Nie Powtarzaj Się) oraz SRP (Single Responsibility Principle - Zasada Jednej Odpowiedzialności), co oznacza, że każda funkcja robi jedną rzecz, ale robi ją dobrze.

## Zobacz również
Aby dowiedzieć się więcej o funkcjach w C++:
- https://pl.cppreference.com/w/cpp/language/functions
- https://www.learncpp.com/cpp-tutorial/77-introduction-to-functions/

Aby dowiedzieć się więcej o zasadach projektowania związanych z funkcjami:
- https://pl.wikipedia.org/wiki/Zasada_jednej_odpowiedzialności
- https://pl.wikipedia.org/wiki/Nie_powtarzaj_się

Dowiedz się więcej na temat lambd i zaawansowanego użycia funkcji:
- https://www.cprogramming.com/c++11/c++11-lambda-closures.html
- https://isocpp.org/wiki/faq/cpp14-language#lambda-captures
