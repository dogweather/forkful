---
title:                "Generowanie liczb losowych"
html_title:           "Gleam: Generowanie liczb losowych"
simple_title:         "Generowanie liczb losowych"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Generowanie losowych liczb to proces tworzenia liczb, które nie mają żadnego przewidywalnego wzorca. Programiści robią to, aby dodać element nieprzewidywalności do swoich aplikacji, co jest niezbędne w grach, symulacjach czy testach bezpieczeństwa.

## Jak to zrobić:

W Bashu możemy wygenerować losową liczbę korzystając z predefiniowanej zmiennej `$RANDOM`. Poniżej znajduje się przykładowy skrypt:

```Bash
echo $RANDOM
```

Wywołanie tego skryptu da nam losową liczbę pomiędzy 0 a 32767. 

Jeśli chcemy losową liczbę w określonym przedziale, np. od 1 do 100, możemy to zrobić następująco:

```Bash
echo $((RANDOM%100+1))
```

## Głębsze spojrzenie:

Zmienna `$RANDOM` w Bashu istnieje od czasów, gdy losowość była rzadkością. Choć jest prosta w użyciu, ma kilka ograniczeń. Najważniejsze to fakt, że generuje tylko liczby z przedziału 0-32767.

Istnieją alternatywne metody generowania losowych liczb w Bashu. Jedną z nich jest korzystanie z `/dev/urandom` lub `/dev/random`.

Implementacje `$RANDOM` różnią się w zależności od systemu i bashowej wersji. `$RANDOM` generuje pseudolosowe liczby zgodnie z liniowym kongruentnym generatorem liczb losowych.

## Zobacz też:

Szczegółowy artykuł na temat generowania liczb losowych w Bashu: https://www.linuxjournal.com/content/generate-random-numbers-bash 

Informacje o `$RANDOM` na stronie Bash Shell Scripting Wiki: https://bash.cyberciti.biz/guide/$RANDOM 

Bashowa dokumentacja dla `$RANDOM`: https://www.gnu.org/software/bash/manual/bash.html#Shell-Variables.