---
title:    "Kotlin: Tests schreiben."
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Warum

Tests sind ein wichtiger Bestandteil des Softwareentwicklungsprozesses, da sie sicherstellen, dass unser Code korrekt funktioniert und potenzielle Fehler identifizieren. Durch das Schreiben von Tests können wir auch sicherstellen, dass Änderungen an unserem Code oder neuen Funktionen keine unerwarteten Auswirkungen haben.

## Wie man Tests schreibt

Um in Kotlin Tests zu schreiben, verwenden wir das in der Standardbibliothek enthaltene `kotlin.test` Paket. Hier ist ein Beispiel, wie wir eine einfache Funktion `add` testen können, die zwei Zahlen addiert:

```Kotlin
fun add(x: Int, y: Int): Int {
  return x + y
}
```

```Kotlin
@Test
fun testAdd() {
  val result = add(2, 2)
  assertEquals(4, result)
}
```

In diesem Codeblock importieren wir das `kotlin.test` Paket und verwenden die `@Test` Annotation, um unseren Testfall zu definieren. In der Testfunktion rufen wir dann unsere `add` Funktion auf und überprüfen mit der `assertEquals` Funktion, ob das Ergebnis 4 ist.

Hier ist ein weiteres Beispiel, wie wir das `kotlin.test` Paket verwenden können, um einen Testfall für eine Funktion zu schreiben, die eine Liste sortiert:

```Kotlin  
fun sortList(list: List<Int>): List<Int> {
  return list.sorted()
}
```

```Kotlin
@Test
fun testSortList() {
  val unsortedList = listOf(3, 5, 1, 2, 4)
  val sortedList = sortList(unsortedList)
  assertEquals(listOf(1, 2, 3, 4, 5), sortedList)
}
```

Wir verwenden die `@Test` Annotation erneut, um unseren Testfall zu definieren. Dann erstellen wir eine Liste mit unsortierten Zahlen und übergeben sie an unsere `sortList` Funktion. Mit der `assertEquals` Funktion überprüfen wir, ob die zurückgegebene Liste tatsächlich sortiert ist.

## Tiefergehende Informationen zu Tests

Es gibt verschiedene Arten von Tests, die in der Softwareentwicklung verwendet werden können, wie z.B. Unittests, Integrationstests und End-to-End-Tests. Unittests testen einzelne Funktionen oder Klassen, während Integrationstests die Zusammenarbeit mehrerer Komponenten überprüfen. End-to-End-Tests simulieren die tatsächliche Benutzerinteraktion mit der Anwendung.

Beim Schreiben von Tests ist es wichtig, den Code so einfach wie möglich zu halten und nur auf die gewünschte Funktionalität zu testen. Durch das Schreiben von sauberem und verständlichem Testcode können wir sicherstellen, dass unsere Tests leichter zu warten und zu verstehen sind.

## Siehe auch

- [Die offizielle Kotlin Test Dokumentation](https://kotlinlang.org/api/latest/kotlin.test/)
- [Der Nutzen von Tests für die Softwarequalität](https://www.codecentric.de/wissen/automatisierte-tests-sind-unverzichtbar-fuer-die-softwarequalitaet/) (auf Deutsch)
- [Best Practices für das Schreiben von Tests in Kotlin](https://medium.com/swlh/how-to-write-tests-in-kotlin-3b222441feb5) (auf Englisch)