---
title:    "Kotlin: Wyszukiwanie i zastępowanie tekstu"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

Być może już pracujesz z językiem programowania Kotlin lub masz zamiar zacząć. W obu przypadkach, znajomość podstawowych operacji, takich jak wyszukiwanie i zastępowanie tekstu, może być niezbędna w procesie tworzenia aplikacji. W tym artykule dowiesz się, jak wykorzystać dedykowane funkcje w Kotlin do wyszukiwania i zastępowania tekstu w prosty i wydajny sposób.

## Jak to zrobić?

### Wyszukaj i zastąp

```Kotlin
val originalText = "Witaj, świecie!"
val newText = originalText.replace("świecie", "Kotlin")
println(newText)
```

**Output:** Witaj, Kotlin!

Powyższy fragment kodu używa funkcji `replace()` do wyszukiwania i zastępowania tekstu w zmiennej `originalText`. Napis po prawej stronie znaku `=` w funkcji `replace()` jest tekstem, który chcemy zastąpić, a napis po lewej stronie jest jego zastępcą.

### Wyszukaj i zastąp wszystkie wystąpienia

```Kotlin
val originalText = "Kotlin jest wspaniały język programowania. Ten artykuł jest napisany w Kotlinie."
val newText = originalText.replace("Kotlin", "Java")
println(newText)
```

**Output:** Java jest wspaniały język programowania. Ten artykuł jest napisany w Javie.

Jeśli chcemy zastąpić wszystkie wystąpienia danego tekstu, możemy użyć funkcji `replace()` z parametrem `Regex`:

```Kotlin
val originalText = "Kotlin jest wspaniały język programowania. Ten artykuł jest napisany w Kotlinie."
val newText = originalText.replace(Regex("Kotlin"), "Java")
println(newText)
```

**Output:** Java jest wspaniały język programowania. Ten artykuł jest napisany w Javie.

W powyższym przykładzie, zamiast tekstu, korzystamy z obiektu `Regex`, który określa wzorzec do wyszukania i zastąpienia.

### Wyszukaj i zastąp z wykorzystaniem dodatkowej funkcji

Kotlin oferuje również inne funkcje do wyszukiwania i zastępowania tekstu, takie jak `replaceFirst()` i `replaceAll()`, które pozwalają na bardziej precyzyjne operacje.

```Kotlin
val originalText = "Kotlin jest wspaniały język programowania. Ten artykuł jest napisany w Kotlinie."
val newText = originalText.replaceFirst("Kotlin", "Java")
println(newText)
```

**Output:** Java jest wspaniały język programowania. Ten artykuł jest napisany w Kotlinie.

## Deep Dive

W przypadku bardziej złożonych operacji na tekście, Kotlin oferuje wiele funkcji i metod związanych z regulaminami wyrażeń regularnych. Dzięki nim możemy określić bardziej skomplikowane wzorce do wyszukania i zastąpienia.

```Kotlin
val originalText = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Curabitur eget velit quis orci cursus tempor a ut felis."
val newText = originalText.replace(Regex("[aeiou]"), "*")
println(newText)
```

**Output:** L*rem *ps*m d*l*r s*t *m*t, c*ns*ct*t*r *dc*p*sc*ng *l*t. C*r*b*t*r *g*t v*l*t q**s *rc* c*rs*s t*mp*r * *t f*l*s.

W powyższym przykładzie użyta została funkcja `replace()` z wyrażeniem regularnym, które wskazuje, że należy zastąpić wszystkie samogłoski znakiem '*'.

## See Also

- Oficjalna dokumentacja Kotlin: https://kotlinlang.org/docs/reference/basic-types.html#strings
- Tutorial na temat operacji na tekście w