---
date: 2024-01-26 01:11:37.998037-07:00
description: "Jak to zrobi\u0107: Wyobra\u017A sobie, \u017Ce piszesz prosty skrypt,\
  \ kt\xF3ry wita u\u017Cytkownik\xF3w."
lastmod: '2024-03-13T22:44:35.937814-06:00'
model: gpt-4-1106-preview
summary: "Wyobra\u017A sobie, \u017Ce piszesz prosty skrypt, kt\xF3ry wita u\u017C\
  ytkownik\xF3w."
title: Organizacja kodu w funkcje
weight: 18
---

## Jak to zrobić:
Wyobraź sobie, że piszesz prosty skrypt, który wita użytkowników:

```Ruby
def greet(name)
  "Witaj, #{name}!"
end

puts greet("Alicja")   # Wyjście: Witaj, Alicja!
puts greet("Bartek")    # Wyjście: Witaj, Bartek!
```

A może obliczasz powierzchnię koła:

```Ruby
def circle_area(radius)
  Math::PI * radius ** 2
end

puts circle_area(5)   # Wyjście: 78.53981633974483
```

Ładniej i łatwiej się z tym pracuje, prawda?

## Bardziej szczegółowo
Koncepcja funkcji, znanych również jako metody w Ruby, nie jest nowa - jest tak stara jak samo programowanie. Wracając do lat 50-tych, podprogramy, jak były nazywane, zostały wprowadzone aby redukować redundancję.

Alternatywy? Jasne, masz kod wbudowany, możesz zastosować OOP z klasami i obiektami, albo pójść w kierunku programowania funkcyjnego z lambdami i procs. Ale funkcje są podstawą uporządkowanego kodu. Chcesz wydajności? Lokalne zmienne w funkcjach działają szybko, a funkcje mogą natychmiast zwracać wartości za pomocą `return`.

Jeśli chodzi o implementację, możesz zdefiniować funkcję za pomocą `def` i zakończyć ją słowem `end`. Możesz ustawić parametry domyślne, użyć operatorów splat dla funkcji o zmiennej liczbie argumentów i więcej. Funkcje mogą być tak proste lub skomplikowane, na jakie tylko masz ochotę.

## Zobacz również
- [Dokumentacja metody Ruby](https://ruby-doc.org/core-2.7.0/Method.html)
- [Nauka programowania od Chrisa Pine'a](https://pine.fm/LearnToProgram/)
- [Praktyczny projekt orientowany obiektowo w Ruby autorstwa Sandi Metz](https://www.poodr.com/)
