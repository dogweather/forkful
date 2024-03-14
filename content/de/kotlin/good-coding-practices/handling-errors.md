---
date: 2024-01-26 00:54:11.869434-07:00
description: "Fehlerbehandlung ist die Art und Weise, wie Ihr Code Probleme bew\xE4\
  ltigt, die w\xE4hrend der Ausf\xFChrung auftreten - wie das Fangen eines unerwarteten\
  \ Balls,\u2026"
lastmod: '2024-03-13T22:44:53.853737-06:00'
model: gpt-4-1106-preview
summary: "Fehlerbehandlung ist die Art und Weise, wie Ihr Code Probleme bew\xE4ltigt,\
  \ die w\xE4hrend der Ausf\xFChrung auftreten - wie das Fangen eines unerwarteten\
  \ Balls,\u2026"
title: Fehlerbehandlung
---

{{< edit_this_page >}}

## Was & Warum?
Fehlerbehandlung ist die Art und Weise, wie Ihr Code Probleme bewältigt, die während der Ausführung auftreten - wie das Fangen eines unerwarteten Balls, ohne ihn fallen zu lassen. Programmierer tun dies, um Abstürze zu verhindern und Benutzern eine reibungslose Erfahrung zu bieten.

## Wie geht das:
Kotlin stellt `try`, `catch`, `finally` und `throw` zur Verfügung, um Fehler zu verwalten. So verwenden Sie sie:

```Kotlin
fun main() {
    val numerator = 10
    val denominator = 0

    try {
        val result = numerator / denominator
        println("Ergebnis: $result")
    } catch (e: ArithmeticException) {
        println("Man kann nicht durch null teilen, Kumpel.")
    } finally {
        println("Das passiert so oder so.")
    }
}
```

Ausgabe:
```
Man kann nicht durch null teilen, Kumpel.
Das passiert so oder so.
```

Wenn im `try`-Block etwas schiefgeht, springt die Ausführung zum `catch`. Dieser fängt den spezifischen geworfenen Fehler ab (`ArithmeticException` in diesem Fall). Der `finally`-Block wird danach ausgeführt - unabhängig vom Ergebnis.

## Tiefergehend
Der `try-catch`-Block gibt es seit den frühen Programmierertagen - er ist wie ein Sicherheitsnetz. Kotlin bietet auch `throw` an, um manuell eine Ausnahme in den Ring zu werfen, und es gibt `finally` für Code, der ausgeführt werden muss - oft handelt es sich um Aufräumarbeiten.

Alternativen umfassen den `Result`-Typ und Koltins `try` als Ausdruck.

```Kotlin
val result: Result<Int> = try {
    Result.success(numerator / denominator)
} catch (e: ArithmeticException) {
    Result.failure(e)
}
```
Dieser Ansatz gibt ein `Result`-Objekt zurück – Sie erhalten entweder einen Erfolg oder einen Fehlschlag, ohne das Drama einer unbehandelten Ausnahme.

Die Umsetzung in Kotlin ist ordentlich, weil Sie `try` wie einen Ausdruck verwenden können, was bedeutet, dass es einen Wert zurückgibt. Optionen wie diese machen die Fehlerbehandlung in Kotlin ziemlich vielseitig. Es geht darum, das richtige Werkzeug für den Job zu wählen, genau wie Sie es in einer Werkstatt tun würden.

## Siehe auch
- Kotlin-Dokumentation über Ausnahmen: [Kotlin-Ausnahmebehandlung](https://kotlinlang.org/docs/exception-handling.html)
- Kotlin-Dokumentation über den `Result`-Typ: [Kotlin Result](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-result/)
- Effektive Java, 3. Auflage, von Joshua Bloch - großartige Einblicke in Ausnahmen, auch wenn es spezifisch für Java ist.
