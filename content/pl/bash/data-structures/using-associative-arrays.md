---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:08.671526-07:00
description: "Tablice asocjacyjne to jakby na\u0142adowane tablice, kt\xF3re pozwalaj\u0105\
  \ u\u017Cywa\u0107 ci\u0105g\xF3w znak\xF3w jako indeks\xF3w zamiast samych liczb\
  \ ca\u0142kowitych. Programi\u015Bci u\u017Cywaj\u0105\u2026"
lastmod: '2024-03-13T22:44:35.575597-06:00'
model: gpt-4-0125-preview
summary: "Tablice asocjacyjne to jakby na\u0142adowane tablice, kt\xF3re pozwalaj\u0105\
  \ u\u017Cywa\u0107 ci\u0105g\xF3w znak\xF3w jako indeks\xF3w zamiast samych liczb\
  \ ca\u0142kowitych. Programi\u015Bci u\u017Cywaj\u0105\u2026"
title: Korzystanie z tablic asocjacyjnych
weight: 15
---

## Co i dlaczego?

Tablice asocjacyjne to jakby naładowane tablice, które pozwalają używać ciągów znaków jako indeksów zamiast samych liczb całkowitych. Programiści używają ich do bardziej złożonych struktur danych, co ułatwia obsługę danych, które nie mieszczą się łatwo w sekwencyjnej liście.

## Jak to zrobić:

Na początek zadeklaruj tablicę asocjacyjną w Bashu:

```Bash
declare -A my_array
```

Następnie możesz zacząć wypełniać ją wartościami, używając ciągów znaków jako kluczy:

```Bash
my_array["name"]="Linux Journal"
my_array["topic"]="Programming"
```

Aby uzyskać dostęp do elementu, użyj jego klucza:

```Bash
echo ${my_array["name"]}  # Wypisze: Linux Journal
```

Iteracja po kluczach i wartościach jest również prosta:

```Bash
for key in "${!my_array[@]}"; do
    echo "$key: ${my_array[$key]}"
done
```

Przykładowy wynik może wyglądać tak:

```
name: Linux Journal
topic: Programming
```

Aby dodać lub zmodyfikować elementy, po prostu przypisz wartość do klucza, podobnie jak przy początkowym wypełnianiu:

```Bash
my_array["readers"]="You"
```

Aby usunąć element, użyj `unset`:

```Bash
unset my_array["topic"]
```

## Pogłębienie

Tablice asocjacyjne zostały wprowadzone w wersji Bash 4.0, co czyni je stosunkowo nowym dodatkiem do języka. Przed ich wprowadzeniem, obsługa tablic z indeksami innymi niż liczby całkowite była uciążliwa, często wymagając obejść lub zewnętrznych narzędzi takich jak `awk` lub `sed`.

Pod spodem Bash implementuje tablice asocjacyjne przy użyciu tablic mieszających. Ta implementacja pozwala na efektywne wyszukiwanie kluczy, które pozostaje stosunkowo stałe niezależnie od rozmiaru tablicy, co jest kluczową cechą dla wydajności wykonania skryptu.

Chociaż tablice asocjacyjne w Bash przynoszą dużo mocy i elastyczności do skryptowania w powłoce, mają one również własny zestaw ograniczeń, takich jak bycie nieco bardziej nieporęcznymi w obsłudze w porównaniu z tablicami w językach wyższego poziomu, takich jak Python lub JavaScript. Dla złożonych zadań manipulacji danymi warto rozważyć użycie zewnętrznych narzędzi lub języków bardziej odpowiednich do pracy.

Jednak dla wielu typowych zadań skryptowych, tablice asocjacyjne stanowią cenne narzędzie w zestawie Bash programisty, umożliwiając tworzenie bardziej czytelnych i łatwiejszych w utrzymaniu skryptów poprzez umożliwienie używania znaczących ciągów znaków zamiast indeksów numerycznych.
