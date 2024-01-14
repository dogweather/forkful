---
title:                "C++: Wyszukiwanie i zamiana tekstu"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

Podczas programowania często zdarza się, że musimy zmienić lub zastąpić pewne części tekstu w naszym kodzie. Jest to często uciążliwe i czasochłonne zadanie, jednak istnieje sposób, aby to zrobić w szybki i skuteczny sposób. Dzięki temu artykułowi dowiecie się, jak wykorzystać funkcję wyszukiwania i zamiany tekstu w języku C++, co znacznie ułatwi pracę programistyczną.

## Jak To Zrobić

Aby przeprowadzić wyszukiwanie i zamianę tekstu w języku C++, musimy użyć funkcji "find" i "replace". Przykładowa składnia kodu wygląda następująco:

```C++
string find = "tekst";
string replace = "nowy_tekst";
string input = "To jest tekst do zmiany";
replace(input.find(find), find.length(), replace);

cout << input;

```

Wyżej przedstawiony kod wykona wyszukanie podanego tekstu ("tekst") w zmiennej "input" i zamieni go na nowy tekst ("nowy_tekst"). Wynik wyświetlony na ekranie będzie wyglądać następująco: "To jest nowy_tekst do zmiany".

## Gleboka Analiza

Funkcja "find" służy do znalezienia pozycji podanego tekstu w zmiennej, a funkcja "replace" służy do zamiany wcześniej znalezionego tekstu na nowy. Warto również zauważyć, że funkcja "find" zwraca liczbę całkowitą, która odpowiada miejscu rozpoczęcia tekstu, a funkcja "replace" nie zwraca żadnego wyniku - po prostu zmienia zmienną "input". W przypadku, gdy funkcja "find" nie odnajdzie podanego tekstu, zwróci wartość "-1", co może być wykorzystane w celu sprawdzenia, czy dany tekst istnieje w zmiennej czy nie.

## Zobacz Również

- [Dokumentacja języka C++ - funkcja find](https://en.cppreference.com/w/cpp/string/basic_string/find)
- [Opis funkcji replace w języku C++](https://www.cplusplus.com/reference/string/string/replace/)
- [Przykładowe zadania z wykorzystaniem funkcji wyszukiwania i zamiany](https://www.hackerrank.com/challenges/c-tutorial-strings/problem)