---
title:    "Bash: Obliczanie daty w przyszłości lub przeszłości"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Dlaczego

Obliczanie daty w przyszłości lub przeszłości może być niezbędne w wielu różnych sytuacjach. Może pomóc w zaplanowaniu ważnych wydarzeń, takich jak urodziny czy wakacje, ale także w prowadzeniu biznesu lub innych działań. Dzięki temu narzędziu możesz uniknąć nieprzyjemnych niespodzianek i lepiej zorganizować swoje życie.

## Jak to zrobić

Aby obliczyć datę w przyszłości lub przeszłości w Bashu, możesz skorzystać z wbudowanych funkcji lub zewnętrznych narzędzi. Jeśli chcesz wyświetlić datę w przyszłości, użyj polecenia `date` wraz z opcją `+%Y-%m-%d`, która pozwoli Ci wprowadzić jedynie rok, miesiąc i dzień. Na przykład:

```Bash
date -d "5 years" +%Y-%m-%d
```

W ten sposób wyświetlisz datę 5 lat od dziś. Jeśli chcesz wyświetlić datę w przeszłości, wystarczy dodać znak minus przed liczbą lat.

```Bash
date -d "-3 months" +%Y-%m-%d
```

Powyższe polecenie wyświetli datę sprzed 3 miesięcy.

## Głębsza analiza

Obliczanie daty w przyszłości lub przeszłości może być trudne, jeśli chcesz uwzględnić np. dni tygodnia lub święta. Dlatego warto zastosować bardziej zaawansowane narzędzia, takie jak biblioteka `dateutils` lub polecenie `cal`. Przykładowo, możesz wykorzystać polecenie `cal` do wyświetlenia kalendarza danego miesiąca:

```Bash
cal 12 2020
```

Możesz także użyć funkcji `dateutils` do dodawania lub odejmowania dni od daty, a także uwzględnić okresy świąteczne czy dni wolne od pracy. Dzięki tym narzędziom możliwości obliczania daty w przyszłości lub przeszłości są nieograniczone.

## Zobacz także

- [Dokumentacja polecenia `date` w Bashu](https://www.gnu.org/software/coreutils/date)
- [Dokumentacja biblioteki `dateutils`](https://www.fresse.org/dateutils/)
- [Opis polecenia `cal`](https://man7.org/linux/man-pages/man1/cal.1.html)