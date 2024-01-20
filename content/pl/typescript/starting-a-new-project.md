---
title:                "Rozpoczynanie nowego projektu"
html_title:           "Bash: Rozpoczynanie nowego projektu"
simple_title:         "Rozpoczynanie nowego projektu"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Wprowadzenie do TypeScript: Jak zaczynać nowy projekt

## Co i dlaczego?
Rozpoczęcie nowego projektu oznacza stworzenie od podstaw nowej aplikacji lub systemu, które będą realizować określone zadania. Programiści to robią aby rozwiązywać nowe problemy, nauczyć się nowych technologii, czy też zaspokoić wymagania klientów.

## Jak to zrobić:
Przykład tworzenia nowego projektu w TypeScript na platformie Node.js z użyciem narzędzia npm (node package manager).

```TypeScript
// krok 1: utworzenie nowego katalogu na projekt
$ mkdir moj-projekt
$ cd moj-projekt

// krok 2: inicjalizacja nowego projektu npm
$ npm init -y

// krok 3: dodanie TypeScript do projektu
$ npm install --save-dev typescript

// krok 4: utworzenie pliku konfiguracyjnego TypeScript (tsconfig.json)
$ npx tsc --init
```

Później, możesz tworzyć pliki .ts (TypeScript) w swoim projekcie i kompilować je do JavaScript używając komendy `tsc`.

## Głębsze spojrzenie:

1. Kontekst historyczny: TypeScript został stworzony przez Microsoft w 2012 roku jako język programowania który dodaje typy statyczne i klasy do JavaScript.

2. Alternatywy: Istnieja inne języki (takie jak Dart czy Elm) które również dodaja silne typowanie do JavaScript, ale TypeScript jest najbardziej popularny ze względu na swoje możliwości i zgodność z JavaScript.

3. Szczegóły implementacji: Po utworzeniu projektu TypeScript, kod jest kompilowany do JavaScript, który jest językiem który może być interpretowany przez wszystkie przeglądarki. Pliki konfiguracyjne TypeScript (tsconfig.json) umożliwiają szczegółowe dostosowanie tego procesu.

## Zobacz również:

1. [Dokumentacja TypeScript](https://www.typescriptlang.org/docs/)
2. [Poradnik npm](https://docs.npmjs.com/)
3. [Krótkie wprowadzenie do TypeScript (tutorial)](https://www.typescripttutorial.net/)