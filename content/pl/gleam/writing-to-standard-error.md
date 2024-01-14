---
title:                "Gleam: Pisanie do standardowego błędu"
simple_title:         "Pisanie do standardowego błędu"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego każdy programista powinien wiedzieć, jak pisać do standardowego błędu? Dla większości osób kodowanie to tylko tworzenie programów, ale często zapominamy o tym, że debugowanie jest równie ważne jak pisanie kodu. Pisanie do standardowego błędu może pomóc nam w identyfikacji i rozwiązywaniu problemów, co przekłada się na lepszą jakość naszego oprogramowania.

## Jak to zrobić

Aby napisać do standardowego błędu w Gleam, możemy skorzystać z wbudowanej funkcji `error/1`. Przykładowy kod wyglądałby następująco:

```Gleam
fn divide(dividend, divisor) {
    if divisor == 0 {
        error("Nie można dzielić przez zero")
    } else {
        dividend / divisor
    }
}

// Wywołanie funkcji z błędnymi argumentami
divide(10, 0)
```

W rezultacie powyższego kodu, w oknie konsoli zobaczymy wiadomość "Nie można dzielić przez zero". Jest to bardzo przydatne, gdy używamy zewnętrznych bibliotek czy operujemy na wartościach, które mogą być nieprawidłowe lub nieobsługiwane przez nasz kod.

## Głębszy zanurzenie

Pisanie do standardowego błędu może także pomóc nam w debagowaniu naszego kodu. Wystarczy umieścić wywołania funkcji `error/1` w różnych miejscach naszego kodu, aby sprawdzić, jakie wartości przechodzą przez nasze funkcje i gdzie może występować problem. Dzięki temu możemy łatwiej zlokalizować błąd i rozwiązać go.

## Zobacz również

- [Dokumentacja Gleam o funkcjach obsługi błędów](https://gleam.run/book/tour/error-handling.html)
- [Oficjalna strona języka Gleam](https://gleam.run/)
- [Poradnik dla początkujących w Gleam](https://gleam.run/book/introduction.html)