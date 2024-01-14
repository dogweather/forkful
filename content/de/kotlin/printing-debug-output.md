---
title:                "Kotlin: Ausgabe von Debug-Daten"
programming_language: "Kotlin"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

# Warum

Beim Programmieren tritt man oft auf Fehler oder unerwartete Ergebnisse. Das Hinzufügen von Debug-Ausgaben (Ausgaben zur Fehlerbehebung) kann dabei helfen, diese schnell zu identifizieren und zu beheben. Mit Kotlin können Sie einfach und effektiv Debug-Ausgaben erstellen.

## Wie man Debug-Ausgaben druckt

Verwenden Sie die Standardbibliotheksfunktion `println()` oder `print()` in Kombination mit der `run()` Funktion, um schnell Debug-Ausgaben zu erstellen. Hier ist ein Beispiel:

```Kotlin
val number = 5
println("Die Nummer ist $number")
```

Dies würde die folgende Ausgabe erzeugen: "Die Nummer ist 5". Beachten Sie die Verwendung von `$`, um die Variable `number` in den String einzufügen.

Sie können auch das `debug()` Statement verwenden, um Debug-Ausgaben zu spezifischen Eigenschaften oder Variablen zu erstellen. Hier ist ein Beispiel:

```Kotlin
val text = "Hallo!"
debug(text)
```

Dies würde die folgende Ausgabe erzeugen: "text = Hallo!". Beachten Sie, dass `debug()` eine Erweiterungsfunktion von `println()` ist, die es ermöglicht, Variablen und Eigenschaften direkt zu übergeben.

## Tiefere Einblicke

Sie können die Debug-Ausgaben auch mithilfe von Anführungszeichen und Dollarzeichen formatieren. Hier ist ein Beispiel:

```Kotlin
val radius = 5.0
println("Der Radius beträgt ${"%.2f".format(radius)}")
```

Dies würde die folgende Ausgabe erzeugen: "Der Radius beträgt 5.00". Beachten Sie die Verwendung von `%.2f` innerhalb von `{}`, um den Radius auf zwei Dezimalstellen zu formatieren.

Es ist auch möglich, mehrere Variablen oder Eigenschaften in eine Debug-Ausgabe einzufügen. Hier ist ein Beispiel:

```Kotlin
val vorname = "Max"
val nachname = "Mustermann"
val alter = 25
println("$vorname $nachname, Alter: $alter")
```

Dies würde die folgende Ausgabe erzeugen: "Max Mustermann, Alter: 25".

## Siehe auch

- [Kotlin - Standardbibliothek](https://kotlinlang.org/api/latest/jvm/stdlib/)
- [Kotlin - Debugging](https://kotlinlang.org/docs/reference/debugging.html)