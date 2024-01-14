---
title:    "Kotlin: Usuwanie znaków pasujących do wzorca"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

Często w trakcie pisania kodu, możemy napotkać sytuację, w której musimy usunąć pewne znaki z tekstu, które odpowiadają określonemu wzorcowi. W takich przypadkach, zastosowanie odpowiedniej funkcji do usuwania znaków jest kluczowe dla efektywnego i poprawnego działania naszego programu. W tym artykule przyjrzymy się, jak w języku Kotlin możemy usunąć znaki pasujące do określonego wzorca.

## Jak to zrobić

Przykładowym problemem, z którym możemy się spotkać, jest usuwanie spacji z tekstu. W Kotlinie, możemy użyć metody `replace()` z wyrażeniem regularnym, aby osiągnąć ten cel:

```Kotlin
val text = "To jest przykładowy tekst z dużą ilością spacji."
val newText = text.replace("\\s".toRegex(), "")

println(newText)
// Output: Tojestprzykładowytekstzdużąilościąspacji.
```

W powyższym przykładzie, użyliśmy metody `replace()` wraz z wyrażeniem regularnym `\s`, które oznacza wszystkie białe znaki (takie jak spacja, tabulator, nowa linia). Następnie, wywołując `toRegex()`, konwertujemy wyrażenie regularne na obiekt typu `Regex`, który jest wymagany przez metodę `replace()`. W drugim parametrze podajemy pusty ciąg znaków, co oznacza, że wszystkie znalezione białe znaki zostaną zastąpione pustym ciągiem znaków, czyli efektem końcowym jest tekst bez spacji.

Możemy również użyć metody `filter()` na kolekcji znaków, aby usunąć tylko określone znaki z tekstu. Przykładowo, jeśli chcemy usunąć wszystkie znaki alfabetu z tekstu, możemy napisać:

```Kotlin
val text = "Ten tekst zawiera dużą ilość liter alfabetu."
val newText = text.filter { !it.isLetter() }

println(newText)
// Output: .
```

W powyższym przykładzie, wykorzystaliśmy `filter()` wraz z wyrażeniem lambda, które zwraca tylko znaki, które nie są literami. Dzięki temu, w wynikowym tekście zostaną usunięte wszystkie litery.

## Głębszy rozkład

Podczas usuwania znaków pasujących do określonego wzorca, ważne jest zwrócenie uwagi na ewentualne ograniczenia lub dodatkowe warunki. Na przykład, jeśli chcemy usunąć tylko spacje znajdujące się na początku lub na końcu tekstu, możemy skorzystać z metody `trim()`.

```Kotlin
val text = "  Ten tekst zawiera białe znaki na początku i na końcu.  "
val newText = text.trim()

println(newText)
// Output: Ten tekst zawiera białe znaki na początku i na końcu.
```

Jak widać, metoda `trim()` usuwa białe znaki tylko z początku i końca tekstu, zachowując je w środku. Innym przykładem może być potrzeba zachowania znaków spacji na początku lub końcu oraz usunięcie ich tylko wewnątrz tekstu, co możemy osiągnąć wykorzystując metodę `replace()` z odpowiednim wyrażeniem regularnym.

## Zobacz również

- [Kotlin Reference: String manipulation functions](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string/)
- [Regular expression cheat sheet](https://www.rexegg.com/regex-quickstart.html)