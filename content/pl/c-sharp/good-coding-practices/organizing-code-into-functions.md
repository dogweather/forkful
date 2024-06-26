---
date: 2024-01-26 01:10:01.862902-07:00
description: "Jak to zrobi\u0107: Wyobra\u017A sobie, \u017Ce masz kod, kt\xF3ry kilka\
  \ razy wypisuje powitanie. Bez funkcji jest to ba\u0142agan. Z funkcjami jest to\
  \ uporz\u0105dkowane."
lastmod: '2024-03-13T22:44:35.413829-06:00'
model: gpt-4-1106-preview
summary: "Wyobra\u017A sobie, \u017Ce masz kod, kt\xF3ry kilka razy wypisuje powitanie."
title: Organizacja kodu w funkcje
weight: 18
---

## Jak to zrobić:
Wyobraź sobie, że masz kod, który kilka razy wypisuje powitanie. Bez funkcji jest to bałagan. Z funkcjami jest to uporządkowane.

```C#
// Bez funkcji - powtarzalne
Console.WriteLine("Hello, Amy!");
Console.WriteLine("Hello, Bob!");
Console.WriteLine("Hello, Charlie!");

// Z funkcjami - czyściej
void Greet(string name) {
    Console.WriteLine($"Hello, {name}!");
}

Greet("Amy");
Greet("Bob");
Greet("Charlie");
```

Efekt końcowy jest ten sam, ale druga wersja jest znacznie bardziej uporządkowana.

## Wnikliwe spojrzenie
Dawno temu, w czasach języka asemblera, przeskakiwanie do różnych części kodu odbywało się za pomocą GOTO – było to chaotyczne i trudne do śledzenia. Funkcje to znaczący postęp, jak zorganizowane szuflady w skrzynce z narzędziami. Alternatywy? Oczywiście, masz metody, które są funkcjami w kontekście klasy. Potem są jeszcze lambdy i funkcje wbudowane do szybkich, jednorazowych zadań.

Jeśli chodzi o implementację – małe, skoncentrowane funkcje są cenne. Są łatwiejsze do przetestowania i debugowania. Duże funkcje z wieloma odpowiedzialnościami mogą stać się potworniejskie, zdobywając niechlubny tytuł "kodu spaghetti". Trzymaj się jednego zadania na funkcję; będziesz sobie później wdzięczny.

## Zobacz także
Aby dowiedzieć się więcej o funkcjach i najlepszych praktykach, sprawdź:

- Clean Code autorstwa Roberta C. Martina: Zasady utrzymywania porządku w funkcjach.
- Refactoring autorstwa Martina Fowlera: Sposoby na ulepszanie istniejącego kodu.
- Przewodnik po metodach C# od Microsoftu: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/classes-and-structs/methods
