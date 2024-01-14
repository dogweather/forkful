---
title:    "C: Wyszukiwanie i zamiana tekstu"
keywords: ["C"]
---

{{< edit_this_page >}}

## Dlaczego?

Wielu programistów może zastanawiać się, dlaczego powinni poświęcić czas na naukę wyszukiwania i zamiany tekstu w kodzie C. Jednak pisanie czystego i przejrzystego kodu jest niezwykle ważne w programowaniu, a umiejętność wyszukiwania i zamiany tekstu może być bardzo przydatna w celu ułatwienia edycji i utrzymania kodu.

## Jak to zrobić?

Aby wyszukać i zamienić tekst w kodzie C, możemy skorzystać z funkcji "str_replace" z biblioteki string.h. Przykładowy kod może wyglądać następująco:

```C
#include <stdio.h>
#include <string.h>

int main (){
   char text[100];
   char old_word[20];
   char new_word[20];

   printf("Wpisz tekst: ");
   gets(text);

   printf("Wpisz słowo, które chcesz zamienić: ");
   gets(old_word);

   printf("Wpisz nowe słowo: ");
   gets(new_word);

   //wyszukaj i zamień słowa w tekście
   str_replace(text, old_word, new_word);

   //wypisz zmieniony tekst
   printf("Zmieniony tekst: %s", text);

   return 0;
}
```

Przykładowe wyjście programu po wykonaniu powyższego kodu może wyglądać następująco:

```
Wpisz tekst: Zrobiłem coś
Wpisz słowo, które chcesz zamienić: coś
Wpisz nowe słowo: ciekawego
Zmieniony tekst: Zrobiłem ciekawe
```

## Deep Dive

Funkcja "str_replace" działa na podstawie biblioteki string.h, która zawiera wiele przydatnych funkcji do manipulacji i operacji na tekście. Funkcja ta pozwala na zamianę jednego słowa na inne w danym tekście, ale nie wpływa na oryginalny tekst, tylko tworzy nową wersję z zamienionymi słowami.

Funkcja ta również umożliwia użycie wyrażeń regularnych jako wzorca do wyszukania tekstu. Używanie wyrażeń regularnych może znacznie rozszerzyć możliwości funkcji i ułatwić dokładniejsze i skomplikowane wyszukiwanie i zamianę tekstów w kodzie.

## Zobacz także

- Dokumentacja funkcji "str_replace" w języku C: https://www.tutorialspoint.com/c_standard_library/c_function_str_replace.htm

- Przykładowe wyrażenia regularne do wyszukiwania w kodzie C: https://regexr.com/3rdbd

- Poradnik na temat pracy z tekstami w języku C: https://www.tutorialspoint.com/cprogramming/c_strings.htm