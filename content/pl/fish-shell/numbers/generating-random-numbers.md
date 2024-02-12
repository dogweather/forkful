---
title:                "Generowanie liczb losowych"
aliases: - /pl/fish-shell/generating-random-numbers.md
date:                  2024-01-27T20:33:56.363464-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generowanie liczb losowych"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Generowanie losowych liczb jest podstawowym zadaniem w programowaniu, używanym do wszystkiego - od próbkowania danych po rozwój gier. W Fish Shell użycie narzędzi systemowych i funkcji wbudowanych do tego celu pozwala programistom efektywnie włączać losowość i zmienność do skryptów i aplikacji.

## Jak to zrobić:

Generowanie losowej liczby w Fish może być proste, przy użyciu kombinacji narzędzi systemowych i możliwości powłoki. Poniżej znajdują się przykłady demonstrujące, jak generować losowe liczby w określonych zakresach.

**Generowanie losowej liczby między 0 a 100:**

```fish
set -l rand_num (random 0 100)
echo $rand_num
```

**Przykładowy wynik:**
```fish
42
```

**Generowanie losowej liczby między dowolnymi dwoma liczbami, powiedzmy 50 i 150:**

```fish
set -l min 50
set -l max 150
set -l rand_num (random $min $max)
echo $rand_num
```

**Przykładowy wynik:**
```fish
103
```

**Używanie random do mieszania listy:**

Możesz także chcieć losowo przetasować elementy na liście. Oto jak możesz to zrobić:

```fish
set -l my_list A B C D E
random (seq (count $my_list)) | while read i
    echo $my_list[$i]
end
```

**Przykładowy wynik:**
```fish
C
A
E
D
B
```

Zwróć uwagę, że wynik będzie się różnić za każdym razem, gdy uruchomisz te komendy z powodu natury losowości.

## Głębsze spojrzenie

Funkcja `random` w Fish Shell zapewnia łatwy w użyciu interfejs do generowania liczb pseudolosowych. Wewnętrznie opakowuje ona narzędzia systemowe do generowania liczb losowych, oferując przenośny sposób na wprowadzenie losowości do skryptów. Jednakże, ważne jest, aby pamiętać, że losowość zapewniana przez `random` jest wystarczająca dla większości zadań skryptowych, ale może nie spełniać wymagań bezpieczeństwa kryptograficznego dla aplikacji wymagających wyższego stopnia nieprzewidywalności.

Dla kontekstów bezpieczeństwa o wysokich stawkach, rozważ użycie dedykowanych narzędzi lub bibliotek programistycznych zaprojektowanych dla celów kryptograficznych, które zapewniają mocniejsze gwarancje losowości. Niemniej jednak, dla ogólnego skryptowania i aplikacji, gdzie najwyższe standardy bezpieczeństwa dla losowości nie są wymagane, funkcja `random` w Fish Shell oferuje wygodne i efektywne rozwiązanie.
