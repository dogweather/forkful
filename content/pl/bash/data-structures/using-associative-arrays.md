---
title:                "Korzystanie z tablic asocjacyjnych"
aliases:
- /pl/bash/using-associative-arrays/
date:                  2024-01-30T19:10:08.671526-07:00
model:                 gpt-4-0125-preview
simple_title:         "Korzystanie z tablic asocjacyjnych"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
