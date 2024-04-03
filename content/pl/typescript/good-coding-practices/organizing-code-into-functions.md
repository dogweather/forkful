---
date: 2024-01-26 01:16:23.376107-07:00
description: "Jak to zrobi\u0107: Wyobra\u017A sobie, \u017Ce tworzysz podstawowy\
  \ kalkulator. Zamiast pisa\u0107 logik\u0119 dodawania wsz\u0119dzie tam, gdzie\
  \ jest potrzebna, utw\xF3rz funkcj\u0119 `add`."
lastmod: '2024-03-13T22:44:35.144917-06:00'
model: gpt-4-0125-preview
summary: "Wyobra\u017A sobie, \u017Ce tworzysz podstawowy kalkulator."
title: Organizowanie kodu w funkcje
weight: 18
---

## Jak to zrobić:
Wyobraź sobie, że tworzysz podstawowy kalkulator. Zamiast pisać logikę dodawania wszędzie tam, gdzie jest potrzebna, utwórz funkcję `add`:

```TypeScript
function add(x: number, y: number): number {
  return x + y;
}

console.log(add(5, 7)); // Przykładowy wynik: 12
```

Teraz, powiedzmy, że potrzebujemy funkcji do mnożenia:

```TypeScript
function multiply(x: number, y: number): number {
  return x * y;
}

console.log(multiply(3, 4)); // Przykładowy wynik: 12
```
Zauważ, jak skupiamy się na jednym zadaniu na funkcję? To jest sedno organizowania kodu.

## Dogłębna analiza
Historycznie, w miarę ewolucji języków programowania, funkcje stały się kluczowe w strukturyzowaniu kodu, czerpiąc z funkcji matematycznych. Są one filarem w programowaniu proceduralnym i żyją dalej w paradygmatach programowania obiektowego i funkcjonalnego.

Alternatywy? Można po prostu nie używać funkcji, ale to prosty bilet do Miasta Spaghetti. Albo można pójść w OOP (Programowanie Obiektowe) i zapakować funkcjonalności w metody, które są w zasadzie funkcjami należącymi do obiektów.

Jeśli chodzi o implementację, TypeScript nalega na typy. Definiowanie typów wejściowych i wyjściowych dla funkcji nie jest tylko dobrym manierem; to mus dla czystego kodu TypeScript. Co więcej, dzięki TypeScript, otrzymujesz takie fajne funkcje jak przeciążenia, generyki i opcjonalne parametry, aby superładować swoje funkcje.

## Zobacz również
Sprawdź te zasoby, aby podnieść swój poziom w zakresie funkcji:

- [Podręcznik TypeScript – Funkcje](https://www.typescriptlang.org/docs/handbook/2/functions.html): Twoja Biblia dla funkcji TypeScript.
- [Czysty Kod JavaScript](https://github.com/ryanmcdermott/clean-code-javascript#functions): Zastosuj zasady Czystego Kodu do swoich funkcji JavaScript.
- [Nie Znasz JS – Zakres i Zamknięcia](https://github.com/getify/You-Dont-Know-JS): Zrozum, jak funkcje współpracują ze zakresem i zamknięciami w JavaScript.
