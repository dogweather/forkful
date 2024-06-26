---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:11:06.584128-07:00
description: "Jak to zrobi\u0107: W Elm pracujesz ze S\u0142ownikami w module `Dict`,\
  \ wi\u0119c zanurkujmy w szybki przyk\u0142ad."
lastmod: '2024-03-13T22:44:35.314250-06:00'
model: gpt-4-0125-preview
summary: "W Elm pracujesz ze S\u0142ownikami w module `Dict`, wi\u0119c zanurkujmy\
  \ w szybki przyk\u0142ad."
title: Korzystanie z tablic asocjacyjnych
weight: 15
---

## Jak to zrobić:
W Elm pracujesz ze Słownikami w module `Dict`, więc zanurkujmy w szybki przykład:

```Elm
import Dict exposing (Dict)

-- Inicjalizacja słownika z kluczami typu String i wartościami typu Int
exampleDict : Dict String Int
exampleDict = Dict.fromList [("apple", 5), ("banana", 2), ("orange", 8)]

-- Dodawanie lub aktualizacja wartości
updatedDict = Dict.insert "grape" 10 exampleDict

-- Pobieranie wartości (zauważ typ Maybe, ponieważ klucz może nie być obecny)
fruitCount : Maybe Int
fruitCount = Dict.get "apple" updatedDict

-- Usuwanie pary klucz-wartość
finalDict = Dict.remove "banana" updatedDict

-- Konwersja słownika z powrotem do listy
dictToList = Dict.toList finalDict
```

Przykładowy output przy wyświetlaniu `dictToList`:

```Elm
[("apple", 5), ("grape", 10), ("orange", 8)]
```

To demonstruje podstawowe operacje: tworzenie, aktualizowanie, dostęp i iteracja nad Słownikiem.

## W głąb
Słowniki w Elm wewnętrznie używają struktury znanej jako drzewo AVL - typ samobalansującego się drzewa binarnego. Ten wybór stanowi równowagę między zapewnieniem, że operacje takie jak insert, get i remove mają dobrą wydajność (czas logarytmiczny) oraz zachowaniem prostoty w obsłudze danych.

Pomimo zalet `Dict` w Elm, nie jest to rozwiązanie pasujące do wszystkiego. Dla kolekcji, które są uporządkowane lub muszą być przeglądane sekwencyjnie, Lista lub Tablica może być bardziej odpowiednia. Ponadto, przy pracy z ustalonym zestawem znanych kluczy, użycie typów niestandardowych (wersja Elm enumów) mogłoby zaoferować większe bezpieczeństwo typów i wyraźniejsze intencje w kodzie.

W ekosystemie Elm, `Dict` oferuje niezawodny sposób zarządzania kolekcjami par klucz-wartość, gdzie klucze są unikatowe i kolejność nie ma znaczenia. Chociaż mogą pojawić się nowe lub bardziej zaawansowane struktury, moduł `Dict` pozostaje podstawowym narzędziem w zestawie programisty Elm za jego prostotę i wydajność w obsłudze tablic asocjacyjnych.
