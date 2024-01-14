---
title:                "C: Pisanie do standardowego wyjścia błędu"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Dlaczego warto korzystać z pisania do standardowego błędu?

Każdemu programiście lubiącemu język C z pewnością zdarzyło się już natknąć na konieczność wyświetlenia błędu podczas wykonywania kodu. Podczas analizy problemu często przydatne jest zapisanie komunikatu o błędzie w specjalnym miejscu, aby ułatwić sobie późniejsze debugowanie kodu. W przypadku języka C takim miejscem jest standardowe wyjście błędu, które w prosty sposób pozwala na wypisanie informacji o błędzie na ekranie lub do pliku. W tym blogu dowiesz się, dlaczego warto korzystać z pisania do standardowego błędu oraz jak to zrobić.

## Jak pisać do standardowego błędu w języku C?

Aby napisać do standardowego błędu w języku C, wystarczy skorzystać z funkcji `fprintf()` i przekazać jej jako pierwszy argument `stderr`, czyli wskaźnik na standardowe wyjście błędu. Przykładowo, jeśli chcemy wypisać komunikat "Błąd: nie można otworzyć pliku" do standardowego błędu, to możemy to zrobić takim kodem:

```C
#include <stdio.h>

int main() {
    fprintf(stderr, "Błąd: nie można otworzyć pliku");
    return 0;
}
```

Po uruchomieniu programu, komunikat ten zostanie wypisany na ekranie lub do pliku, w zależności od tego, jak program jest uruchamiany. 

## Głębszy zanurzenie w pisaniu do standardowego błędu

Poza funkcją `fprintf()`, istnieją również inne funkcje, które pozwalają na pisanie do standardowego błędu w języku C. Jedną z nich jest `perror()`, która oprócz przekazanego przez nas komunikatu, wyświetli również komunikat z błędem systemowym zapisanym w zmiennej `errno`. Jest to przydatne, gdy chcemy dokładniej zidentyfikować przyczynę błędu.

Warto także pamiętać, że standardowe wyjście błędu może być przekierowane i niekoniecznie musi być wypisywane na ekranie. Może to być również zapisane do pliku lub przekierowane do innego urządzenia wyjściowego. Dzięki temu możemy wykorzystać standardowe wyjście błędu w bardziej zaawansowany sposób, np. do zapisywania logów błędów do pliku.

## Zobacz także

Jeśli interesuje Cię temat pisania do standardowego błędu w języku C, to polecam zapoznanie się z poniższymi linkami:

- Dokumentacja funkcji `fprintf()` na stronie cppreference.com
- Przykładowe wykorzystanie standardowego wyjścia błędu w celu zapisywania logów błędów do pliku na blogu "The Crazy Programmer"

Dziękujemy za przeczytanie tego bloga i mamy nadzieję, że teraz wiesz, dlaczego warto korzystać z pisania do standardowego błędu w języku C oraz jak to zrobić w praktyce. Życzymy powodzenia w rozwiązywaniu problemów i pisaniu jeszcze lepszego kodu!