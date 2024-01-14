---
title:    "Kotlin: Pisanie do standardowego błędu"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie do standardowego błędu (standard error) często jest wykorzystywane w programowaniu w celu wyświetlania błędów lub ostrzeżeń. Umożliwia to programiście szybkie zlokalizowanie potencjalnych problemów w kodzie i szybką naprawę błędów.

## Jak to zrobić

Pisanie do standardowego błędu w języku Kotlin jest bardzo proste. Wystarczy użyć funkcji "println()" i przekazać jej jako argument obiekt typu "PrintStream", który będzie reprezentował standardowy błąd. Przykład kodu wyglądałby tak:

```Kotlin
println(System.err, "To jest wiadomość błędu.")
```

W wyniku powyższego kodu, na konsoli zostanie wyświetlona wiadomość "To jest wiadomość błędu." z oznaczniem, że pochodzi ona ze standardowego błędu.

Możemy również przekazać do funkcji "println()" dowolny obiekt jako argument, a zostanie on automatycznie przekonwertowany do ciągu znaków. Przykład:

```Kotlin
val x = 5
println(System.err, "Wartość x to: " + x) // Automatyczna konwersja do ciągu znaków
```

Wynik powyższego kodu będzie identyczny jak w poprzednim przykładzie.

## Deep Dive

Pisanie do standardowego błędu jest bardzo przydatne w przypadku, gdy chcemy wyświetlić błędy lub ostrzeżenia nie tylko na konsoli, ale również w dziennikach (logach) lub na innych wynikach wyjściowych. Standardowy błąd jest również wykorzystywany w bibliotekach i innych frameworkach, więc warto znać tę funkcję programowania.

Dzięki wykorzystaniu funkcji "println()", programista może kontrolować wyświetlany komunikat błędu lub ostrzeżenia, co jest bardzo ważne w przypadku tworzenia aplikacji, które będą wykorzystywane przez użytkowników.

## Zobacz także

- Dokumentacja języka Kotlin dotycząca pisania do standardowego błędu: https://kotlinlang.org/docs/reference/standard-library.html#printing-to-the-standard-error-stream
- Informacje na temat obsługi wyjątków w języku Kotlin: https://kotlinlang.org/docs/reference/exceptions.html