---
title:                "Kotlin: Łączenie ciągów znaków"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego
Konkatenacja ciągów znaków jest niezbędnym elementem programowania w Kotlinie, ponieważ pozwala na łączenie różnych fragmentów tekstu w jedną zmienną, co jest bardzo przydatne w wielu zastosowaniach. Bez niej trudno byłoby tworzyć czytelne i skuteczne aplikacje.

## Jak to zrobić
Aby zastosować konkatenację ciągów znaków w swoim kodzie Kotlin, należy użyć operatora plus (+) lub metody plus(). Poniżej znajdują się przykłady, jak można to zrealizować:

```
Kotlin val imie = "Anna"
val nazwisko = "Kowalska"
val witaj = "Witaj w naszej aplikacji, " + imie + " " + nazwisko
println(witaj)
```

W powyższym przykładzie operator plus (+) został użyty do połączenia trzech zmiennych ze znakami spacji między nimi. Wynikiem wywołania metody println() będzie tekst: "Witaj w naszej aplikacji, Anna Kowalska".

```
Kotlin val wiek = 25
val rokUrodzenia = 1996
val dataUrodzenia = "Urodziłeś się w " + (rokUrodzenia - wiek) + " roku."
println(dataUrodzenia)
```

W powyższym przykładzie widoczne jest użycie operatora plus (+) oraz nawiasów, aby wykonać operację matematyczną przed konkatenacją. Wynikiem wywołania metody println() będzie tekst: "Urodziłeś się w 1971 roku.".

## Głębszy wgląd
W Kotlinie konkatenacja ciągów znaków jest obsługiwana przy pomocy mechanizmu zwanego operator overloading, co oznacza, że można zdefiniować zachowanie dla operatora plus (+) zależnie od typu danych, z którymi jest używany. Dzięki temu można np. wykonać konkatenację dwóch list lub obiektów.

String, jako klasa w Kotlinie, udostępnia również różne metody do pracy z ciągami znaków, takie jak replace(), substring(), trim(), format() i wiele innych, co czyni możliwym wygodniejsze i bardziej rozbudowane operacje na tekstach.

## Zobacz również
- [Dokumentacja Kotlin o konkatenacji ciągów](https://kotlinlang.org/docs/basic-types.html#strings)
- [Kotlin dla początkujących: konkatenacja ciągów znaków](https://kotlin.pl/kotlin-dla-poczatkujacych-konkatenacja-ciagow-znakow/)