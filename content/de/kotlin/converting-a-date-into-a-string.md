---
title:    "Kotlin: Eine Datumsumwandlung in eine Zeichenkette"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Warum

Das Umwandeln von Datumsangaben in Zeichenketten ist ein häufiges Problem in der Programmierung. Es kann hilfreich sein, um Daten für die Benutzeranzeige oder für die Speicherung in Datenbanken zu formatieren.

## Wie geht das?

Um ein Datum in eine Zeichenkette umzuwandeln, können wir die vordefinierte Funktion `toString()` verwenden. Diese Funktion nimmt das gewünschte Datumsobjekt als Argument und gibt eine formatierte Zeichenkette zurück.

```Kotlin
// Beispielausgabe
val date = Date(1625716061000) // Erstellt ein neues Datum mit dem Zeitstempel 1625716061000
println(date.toString()) // 2021-07-08
```

Alternativ können wir auch das `DateFormatter`-Objekt verwenden, um ein benutzerdefiniertes Datumsformat zu erstellen.

```Kotlin
// Beispielausgabe
val date = Date(1625233177000) // Erstellt ein neues Datum mit dem Zeitstempel 1625233177000
val formatter = DateTimeFormatter.ofPattern("dd.MM.yyyy") // Erstellt einen benutzerdefinierten Datumsformat
println(date.format(formatter)) // 02.07.2021
```

## Tiefere Einblicke

Bei der Umwandlung von Datum in Zeichenkette ist Vorsicht geboten, da das gewählte Format je nach Land oder Region unterschiedlich sein kann. Daher ist es wichtig, das richtige `Locale` zu verwenden, um sicherzustellen, dass das Datum korrekt formatiert wird.

```Kotlin
// Beispielausgabe
val date = Date(1625300400000) // Erstellt ein neues Datum mit dem Zeitstempel 1625300400000
val formatter = DateTimeFormatter.ofPattern("dd.MM.yyyy", Locale.GERMANY) // Verwendet das Locale Deutschland
println(date.format(formatter)) // 03.07.2021
```

Zusätzlich können wir auch Zeitzonen angeben, um das Datum in verschiedenen Zeitzonen korrekt darzustellen.

```Kotlin
// Beispielausgabe
val date = Date(1625739600000) // Erstellt ein neues Datum mit dem Zeitstempel 1625739600000
val formatter = DateTimeFormatter.ofPattern("dd.MM.yyyy HH:mm", Locale.US) // Verwendet das Locale USA und fügt Uhrzeit hinzu
formatter.withZone(ZoneId.of("Europe/Berlin")) // Gibt das Datum in der Zeitzone von Berlin aus
println(date.format(formatter)) // 08.07.2021 14:00
```

## Siehe auch

- [Java ZoneDateTime Klasse](https://www.javatpoint.com/java-zoneddatetime)
- [Kotlin Datums- und Zeitzonen-API](https://kotlinlang.org/docs/datetime.html)