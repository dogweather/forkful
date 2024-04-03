---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:38.636232-07:00
description: "Jak to zrobi\u0107: Tworzenie i u\u017Cywanie tablic asocjacyjnych w\
  \ PowerShellu jest ca\u0142kiem proste. Oto jak to zrobisz: **Tworzenie tablicy\
  \ asocjacyjnej:**."
lastmod: '2024-03-13T22:44:35.620732-06:00'
model: gpt-4-0125-preview
summary: "Tworzenie i u\u017Cywanie tablic asocjacyjnych w PowerShellu jest ca\u0142\
  kiem proste."
title: Korzystanie z tablic asocjacyjnych
weight: 15
---

## Jak to zrobić:
Tworzenie i używanie tablic asocjacyjnych w PowerShellu jest całkiem proste. Oto jak to zrobisz:

**Tworzenie tablicy asocjacyjnej:**

```PowerShell
$myAssociativeArray = @{}
$myAssociativeArray["name"] = "Alex"
$myAssociativeArray["age"] = 25
$myAssociativeArray["job"] = "Inżynier"
```

Ten fragment kodu tworzy tablicę asocjacyjną z trzema parami klucz-wartość.

**Dostęp do wartości:**

Aby uzyskać wartość, odwołaj się do jej klucza:

```PowerShell
Write-Output $myAssociativeArray["name"]
```

**Przykładowe wyjście:**

```
Alex
```

**Dodawanie lub modyfikowanie danych:**

Po prostu użyj klucza, aby dodać nową parę lub zmodyfikować istniejącą:

```PowerShell
$myAssociativeArray["location"] = "Nowy Jork" # Dodaje nową parę klucz-wartość
$myAssociativeArray["job"] = "Starszy Inżynier" # Modyfikuje istniejącą parę
```

**Iterowanie przez tablicę asocjacyjną:**

Przejrzyj klucze i wartości w ten sposób:

```PowerShell
foreach ($key in $myAssociativeArray.Keys) {
  $value = $myAssociativeArray[$key]
  Write-Output "$key : $value"
}
```

**Przykładowe wyjście:**

```
name : Alex
age : 25
job : Starszy Inżynier
location : Nowy Jork
```

## Dogłębna analiza
Koncepcja tablic asocjacyjnych jest wspólna dla wielu języków programowania, zwykle nazywana słownikiem, mapą lub tabelą mieszającą w zależności od języka. W PowerShellu, tablice asocjacyjne są implementowane jako tablice mieszające, które są dość wydajne pod kątem wyszukiwania kluczy, przechowywania danych i utrzymywania kolekcji unikalnych kluczy.

Historycznie, tablice asocjacyjne zapewniały sposób na zarządzanie kolekcjami obiektów, gdzie każdy element może być szybko odzyskany bez przeglądania całej kolekcji, używając jego klucza. Wydajność odzyskiwania i modyfikacji danych w tablicach asocjacyjnych sprawia, że są one preferowanym wyborem do różnych zadań. Mają jednak ograniczenia, takie jak utrzymanie porządku, dla których lepszą alternatywą mogą być uporządkowane słowniki lub niestandardowe obiekty.

Pomimo swoich ograniczeń, tablice asocjacyjne/tablice mieszające w PowerShellu są niezwykle elastyczne i potężnym narzędziem do skryptów. Pozwalają na dynamiczne przechowywanie danych i są szczególnie przydatne w konfiguracjach, manipulacji danych i wszędzie tam, gdzie potrzebny jest uporządkowany format danych bez obciążeń związanych z formalną definicją klasy. Pamiętaj tylko, że choć tablice asocjacyjne są doskonałe do odzyskiwania opartego na kluczu, jeśli twoje zadanie obejmuje złożone struktury danych lub wymaga utrzymania określonego porządku, możesz chcieć zbadać inne typy danych lub niestandardowe obiekty w PowerShellu.
