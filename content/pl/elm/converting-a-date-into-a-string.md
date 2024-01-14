---
title:                "Elm: Konwersja daty na ciąg znaków"
programming_language: "Elm"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Dlaczego

W tym krótkim wpisie dowiesz się, jak w języku Elm konwertować datę na ciąg znaków. Jeśli programujesz w Elm, prawdopodobnie napotkałeś tę sytuację wcześniej i szukasz sposobu, aby to zrobić szybko i skutecznie. Przeczytaj dalej, aby dowiedzieć się, jak to zrobić!

## Jak Konwertować Datę na Ciąg Znaków

Konwertowanie daty na ciąg znaków może być przydatne w wielu sytuacjach, szczególnie przy pracy z formularzami lub wyświetlaniem dat w czytelny i spójny sposób. Wystarczy wykorzystać funkcję `toString`, aby to osiągnąć. Sprawdź poniższy przykład kodu:

```Elm
import Date exposing (Day, Month, Year, Date)
import Date.Format exposing (format)
import Time exposing (millisecond)

date : Date
date = Date.fromCalendarDate { day = 25, month = December, year = 2021 }

toStringDate : String
toStringDate = Date.toString "dd.MM.yyyy" (round (millisecond date))

```

W tym przykładzie użyto funkcji `fromCalendarDate` aby utworzyć obiekt daty, który następnie przekazano jako argument do funkcji `toString`. Drugim argumentem jest format, w jakim chcemy wyświetlić datę. W tym przypadku użyliśmy formatu "dd.MM.yyyy". Ważne jest również zaokrąglanie liczby milisekund, aby uniknąć błędów konwersji.

Po wykonaniu powyższego kodu, zmienna `toStringDate` będzie miała wartość "25.12.2021". Proste, prawda?

## Głębszy Wgląd

Funkcja `toString` ma również inne możliwości konwersji daty. Możesz użyć znaczników, aby wyświetlić różne formaty daty lub czasu. Na przykład:

- "hh:mm" wyświetli czas w formacie 24-godzinnym z dokładnością do minuty.
- "hh:mm:ss a" wyświetli czas 12-godzinny z dokładnością do sekundy i informacją o pory dnia (AM/PM).
- "HH:mm:ss.SSS" wyświetli czas w formacie 24-godzinnym z dokładnością do milisekundy.

Możesz również wyświetlić informacje o dacie, takie jak dzień tygodnia czy numer tygodnia w roku. Więcej szczegółów na temat dostępnych znaczników znajdziesz w [dokumentacji Elm](https://package.elm-lang.org/packages/elm/time/latest/Time#format).

## Zobacz też

- [Dokumentacja Elm - moduł Date](https://package.elm-lang.org/packages/elm/time/latest/Date)
- [Dokumentacja Elm - moduł Time](https://package.elm-lang.org/packages/elm/time/latest/Time)
- [Elm Weekly - polskie wydanie newslettera o języku Elm](http://elmweekly.pl/?#)

Nadzieję, że ten wpis okazał się dla Ciebie przydatny. Jeśli chcesz dowiedzieć się więcej o języku Elm, polecamy zajrzeć do powyższych źródeł oraz do polskiego wydania newslettera Elm Weekly. Happy coding!