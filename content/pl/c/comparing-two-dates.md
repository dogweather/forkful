---
title:                "C: Porównywanie dwóch dat"
programming_language: "C"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Porównywanie dat jest częstym zadaniem w programowaniu, szczególnie w przypadku tworzenia aplikacji, w których ważne jest śledzenie czasu i daty. Umiejętność porównywania dwóch dat jest niezbędna do przeprowadzenia różnych operacji, takich jak wyświetlanie informacji o wydarzeniach, sortowanie danych lub wyświetlanie rekordów w odpowiedniej kolejności. W tym artykule dowiesz się, jak porównywać daty za pomocą języka C.

## Jak to zrobić

Aby porównać dwie daty w języku C, musimy najpierw zrozumieć, że daty są przechowywane w postaci struktury. Struktura ta składa się z pól takich jak dzień, miesiąc, rok itp. W celu porównania dat, musimy utworzyć dwie instancje tej struktury i przypisać do nich wartości, które chcemy porównać.

Następnie możemy wykorzystać operator "==" do porównania dwóch dat. Na przykład, jeśli mamy dwie daty zapisane w strukturze data d1 i d2, możemy użyć poniższego warunku, aby sprawdzić, czy są one równe:

```
if (d1.dzien == d2.dzien && d1.miesiac == d2.miesiac && d1.rok == d2.rok) {
	printf("Daty są równe");
}
```

Uwaga: Pamiętaj, że w przypadku porównywania dat, musimy porównać wszystkie pola, a nie tylko jedno, ponieważ jedna data może mieć inny dzień, ale ten sam miesiąc i rok, co druga data.

Możemy również wykorzystać funkcję `strcmp()` do porównania dat. Ta funkcja porównuje dwa ciągi znaków i zwraca wartość 0, jeśli są one identyczne. W ten sposób można zmodyfikować warunek powyżej, aby wykorzystać funkcję `strcmp()` do porównywania dat.

## Deep Dive

Zrozumienie zasad przechowywania dat w języku C jest ważne podczas porównywania dat, ponieważ wpływa to na sposób, w jaki musimy porównywać różne elementy daty, takie jak dzień, miesiąc czy rok. W przypadku daty, która jest przechowywana w postaci liczby całkowitej, należy pamiętać, że miesiące są liczone od zera, czyli styczeń to wartość 0, luty to 1 itd. Ponadto, rokiem 0 jest rok przed naszą erą, a nie rok 2000.

Ważne jest również unikanie błędów podczas wprowadzania dat do programu. Należy upewnić się, że data jest poprawna, a pola takie jak dzień czy miesiąc nie są przekraczane. W przeciwnym razie może to prowadzić do nieprawidłowych porównań i błędów w naszym programie.

## Zobacz także

- [Porównywanie dat w języku C na przykładzie funkcji `strcmp()`](https://www.tutorialsduniya.com/compare-dates-using-strcmp-function-in-c/)
- [Porównywanie dat w różnych językach programowania](https://www.guru99.com/date-time-and-calendar.html#6)
- [Przydatny przewodnik po porównywaniu dat w języku C++](https://www.cplusplus.com/reference/ctime/tm/)