---
title:    "Kotlin: Korzystanie z wyrażeń regularnych"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Dlaczego korzystać z wyrażeń regularnych w Kotlinie?

Wyrażenia regularne są bardzo przydatnym narzędziem w programowaniu. Pozwalają one na sprawne i precyzyjne wyszukiwanie oraz manipulację tekstem. W Kotlinie, wyrażenia regularne są dostępne dzięki wykorzystaniu klasy Regex. Jest to nieodłączna część języka, więc warto poznać ich działanie i wykorzystać je w swoich projektach.

## Jak używać wyrażeń regularnych w Kotlinie?

Aby rozpocząć używanie wyrażeń regularnych w Kotlinie, należy najpierw utworzyć instancję klasy Regex, która będzie zawierać nasze wyrażenie. Następnie, możemy wywołać na niej metodę find(), która zwróci obiekt typu MatchResult. Za pomocą tej klasy, możemy sprawdzić, czy wyrażenie pasuje do podanego tekstu oraz odczytać dopasowany fragment. Przykładowy kod wykorzystujący wyrażenia regularne w celu wyszukania wszystkich liter w danym tekście wyglądałby następująco:
```Kotlin
val regex = Regex("[a-z]")
val input = "Kotlin jest super!"
val result = regex.find(input)
result?.value // zwróci "otlin"
```

## Głębszy wgląd w wykorzystanie wyrażeń regularnych w Kotlinie

Wyrażenia regularne są bardzo potężnym narzędziem oraz oferują wiele możliwości, jeśli chodzi o manipulację tekstem. Można wykorzystać je do wyszukiwania, zastępowania, wydzielenia części tekstu, a nawet do walidacji danych. W Kotlinie, możemy również wykorzystać wyrażenia regularne wraz z wyrażeniami lambda. Przykład korzystający z tego połączenia wyglądałby następująco:
```Kotlin
val inputList = listOf("kot", "pies", "królik")
val regex = Regex("^[kpr]")
val filteredList = inputList.filter { regex.matches(it) }
filteredList // zwróci ["kot", "pies"]
```

## Zobacz również

- Dokumentacja wyrażeń regularnych w Kotlinie: https://kotlinlang.org/docs/regexp.html
- Kurs internetowy na temat wyrażeń regularnych w Kotlinie: https://www.udemy.com/course/kotlin-regular-expressions
- Biblioteka Regex dla Kotlin: https://github.com/kotlin/ktor/blob/master/ktor-utils/jvm/src/io/ktor/util/Regex.kt