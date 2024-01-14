---
title:    "Kotlin: Konwertowanie ciągu znaków na małe litery"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Dlaczego warto przekonwertować string na małe litery?

Czasami w pracy z programowaniem musimy mieć kontrolę nad wyglądem naszego tekstu. Przekonwertowanie stringa na małe litery może nam ułatwić porównywanie i sprawdzanie równości ciągów znaków, a także ułatwić wyświetlanie tekstu w czytelny sposób.

## Jak to zrobić?

Miła wiadomość – w języku Kotlin konwersja stringa na małe litery jest bardzo prosta. Wystarczy użyć metody `toLowerCase()` na naszym stringu. Przykładowy kod:

```Kotlin
val name = "JAN KOWALSKI"
println(name.toLowerCase())
```
Wynik: `jan kowalski`

Ale co, jeśli w naszym stringu są polskie znaki? W tym przypadku warto użyć metody `toLowerCase(Locale.forLanguageTag("PL"))` z odpowiednim regionem językowym. Przykładowy kod:

```Kotlin
val name = "ŁUKASZ NOWAK"
println(name.toLowerCase(Locale.forLanguageTag("PL")))
```
Wynik: `łukasz nowak`

## Głębszy wgląd

Metoda `toLowerCase()` działa na podstawie domyślnej lokalizacji urządzenia, co może prowadzić do nieoczekiwanych wyników dla różnych języków. Dlatego warto użyć metody `toLowerCase(Locale)` lub `toLowerCase(Locale.forLanguageTag("PL"))`, gdzie możemy jawnie określić język, na który chcemy przekonwertować nasz string. 

## Zobacz także

- Dokumentacja języka Kotlin: https://kotlinlang.org/docs/reference/basic-syntax.html#strings
- Rozmowy o Koltin: https://www.getfretboard.com/topic/kotlin-language/
- Curso de Kotlin: https://www.youtube.com/playlist?list=PLwyncZJOqB2iSQfxuNtTI9Qtq3ixvlHfZ (w języku hiszpańskim)