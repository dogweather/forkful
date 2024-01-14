---
title:    "Kotlin: Odczytywanie pliku tekstowego"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach programowanie jest bardzo popularne. Wiele osób decyduje się na naukę tego zawodu, ponieważ jest to obszar, w którym można znaleźć wiele możliwości i perspektyw rozwoju. Jedną z ważnych umiejętności w programowaniu jest czytanie plików tekstowych. W tym artykule dowiesz się dlaczego jest to istotne oraz jak to zrobić w języku Kotlin.

## Jak to zrobić

Aby czytać pliki tekstowe za pomocą języka Kotlin, musimy najpierw poznać podstawy obsługi plików w tym języku. Najważniejsze funkcje, które pomogą nam w czytaniu plików tekstowych to `File()` i `readLines()`. Za pomocą tych funkcji możemy stworzyć obiekt pliku i odczytać jego zawartość w postaci listy wierszy.

```Kotlin
val file = File("myFile.txt")
val lines = file.readLines()
println(lines)
```

W powyższym przykładzie utworzyliśmy obiekt pliku `myFile.txt` i odczytaliśmy jego zawartość do zmiennej `lines`. Następnie za pomocą funkcji `println` wyświetliliśmy zawartość pliku na ekranie konsoli.

## Deep Dive

Teraz przejdziemy do głębszego zanurzenia w temat czytania plików tekstowych w języku Kotlin. Istnieje kilka sposobów na odczytywanie plików tekstowych, które różnią się od siebie sposobem dostępu do danych. Możemy użyć funkcji `readText()` do odczytania całego pliku jako jednego ciągu znaków lub `useLines()` do odczytania wiersz po wierszu.

```Kotlin
val file = File("myFile.txt")
val text = file.readText()
println(text)
```

W powyższym przykładzie użyliśmy funkcji `readText()` do odczytania całego pliku jako jednego ciągu znaków i wyświetlenia go na ekranie. Jeśli chcemy odczytać plik wiersz po wierszu, możemy użyć funkcji `useLines()` i wywołać funkcję dla każdego wiersza z osobna.

```Kotlin
val file = File("myFile.txt")
file.useLines { lines ->
    lines.forEach { println(it) }
}
```

## Zobacz również

- [Kotlin — read text file](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/read-text.html)
- [Kotlin — useLines()](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/use-lines.html)
- [Podstawy czytania i pisania plików w języku Kotlin](https://www.callicoder.com/kotlin-create-write-read-file/)

Czytanie plików tekstowych jest ważną umiejętnością w świecie programowania. Dzięki językowi Kotlin możemy w łatwy sposób odczytać zawartość pliku i przetworzyć ją zgodnie z naszymi potrzebami. Mam nadzieję, że ten artykuł był dla Ciebie przydatny i pomoże Ci w nauce czytania plików tekstowych w języku Kotlin. Powodzenia!