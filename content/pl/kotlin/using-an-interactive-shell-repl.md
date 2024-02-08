---
title:                "Korzystanie z interaktywnego shella (REPL)"
aliases:
- pl/kotlin/using-an-interactive-shell-repl.md
date:                  2024-01-26T04:15:51.596097-07:00
model:                 gpt-4-0125-preview
simple_title:         "Korzystanie z interaktywnego shella (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
REPL (Read-Eval-Print Loop - Pętla Czytaj-Wykonaj-Wypisz) to proste, interaktywne środowisko programistyczne. Programiści używają go do szybkich prób kodowania, testowania fragmentów kodu lub nauki składni języka bez tworzenia pełnej aplikacji.

## Jak to zrobić:
Uruchomienie REPL Kotlin jest bardzo proste. Otwórz terminal i wpisz `kotlinc`. Trafić powinieneś do powłoki Kotlin. Spróbujmy zdefiniować zmienną i wydrukować jej wartość:

```kotlin
Witaj w Kotlin wersja 1.7.10 (JRE 1.8.0_292-b10)
Wpisz :help dla pomocy, :quit aby zakończyć
>>> val przywitanie = "Cześć, Kotlin REPL!"
>>> println(przywitanie)
Cześć, Kotlin REPL!
```

## Pogłębienie
REPL Kotlin zadebiutował wraz z językiem, aby zachęcić do eksperymentowania. Jest podobny do interaktywnej powłoki Pythona, ale przystosowany do składni i specyfiki Kotlin. Alternatywy? Interaktywne środowiska w IDE, takich jak IntelliJ IDEA, oraz internetowe place zabaw Kotlin. REPL działa przez kompilację kodu „na żywo”, zapewniając natychmiastową informację zwrotną – co jest kluczowe dla nauki i debugowania.

## Zobacz również
- Dokumentacja Kotlin na temat REPL: [https://kotlinlang.org/docs/command-line.html#run-the-repl](https://kotlinlang.org/docs/command-line.html#run-the-repl)
- Wypróbuj Kotlin w przeglądarce: [https://play.kotlinlang.org](https://play.kotlinlang.org)
- Wtyczka JetBrains Kotlin Playground dla IntelliJ IDEA.
