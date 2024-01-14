---
title:    "Kotlin: Lesen von Befehlszeilenargumenten"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Warum

Beim Programmieren gibt es viele verschiedene Ansätze, um Benutzereingaben zu verarbeiten. Eine davon ist das Lesen von Befehlszeilenargumenten, die dem Programm beim Start übergeben werden. In diesem Beitrag werden wir uns anschauen, warum es hilfreich sein kann, Befehlszeilenargumente einzulesen und wie man das in Kotlin umsetzen kann.

## Wie geht man vor?

Um Befehlszeilenargumente in Kotlin einzulesen, verwenden wir die Methode `args` in der `main` Funktion. Diese Methode gibt ein Array von Strings zurück, in dem die übergebenen Argumente gespeichert sind. Im Folgenden sehen wir ein Beispiel:

```Kotlin
fun main(args: Array<String>) {
    println("Folgende Argumente wurden eingelesen:")
    args.forEach { arg -> println(arg) }
}
```

Wenn wir nun unser Programm mit den Argumenten "Hallo" und "Welt" starten, sehen wir folgende Ausgabe:

```
Folgende Argumente wurden eingelesen:
Hallo
Welt
```

Es ist wichtig zu beachten, dass Befehlszeilenargumente immer als Strings eingelesen werden. Wenn wir also beispielsweise eine Zahl als Argument angeben, müssen wir diese erst in den entsprechenden Datentyp umwandeln.

## Tiefer in die Materie

Es ist auch möglich, Befehlszeilenargumente mit Namen zu versehen, um sie besser unterscheiden zu können. Dazu können wir die Apache Commons CLI Bibliothek verwenden. Diese bietet verschiedene Methoden an, um Argumente zu definieren und auszulesen. Ein Beispiel für die Verwendung von Apache Commons CLI sieht folgendermaßen aus:

```Kotlin
fun main(args: Array<String>) {
    val options = Options()
    options.addOption("u", "username", true, "Benutzername")
    options.addOption("p", "password", true, "Passwort")

    val parser = DefaultParser()
    val cmd = parser.parse(options, args)

    val username = cmd.getOptionValue("username")
    val password = cmd.getOptionValue("password")

    println("Benutzername: $username")
    println("Passwort: $password")
}
```

Wenn wir nun unser Programm mit den Argumenten "-u Benutzer -p Passwort" starten, erhalten wir folgende Ausgabe:

```
Benutzername: Benutzer
Passwort: Passwort
```

Die Apache Commons CLI Bibliothek bietet noch viele weitere Funktionen und Optionen, um die Verarbeitung von Befehlszeilenargumenten noch komfortabler zu gestalten.

# Siehe auch

- [Apache Commons CLI Bibliothek](https://commons.apache.org/proper/commons-cli/)
- [Offizielle Kotlin Dokumentation zum Verarbeiten von Befehlszeilenargumenten](https://kotlinlang.org/docs/tutorials/command-line.html)