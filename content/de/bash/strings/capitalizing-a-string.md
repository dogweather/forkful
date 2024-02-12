---
title:                "Einen String großschreiben"
date:                  2024-02-03T19:04:48.107457-07:00
model:                 gpt-4-0125-preview
simple_title:         "Einen String großschreiben"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?
Das Kapitalisieren eines Strings in Bash beinhaltet die Umwandlung des ersten Buchstabens des Strings in einen Großbuchstaben, während der Rest des Strings unverändert bleibt. Diese Technik wird häufig verwendet, um die Ausgabe zu formatieren oder um Kodierungskonventionen zu entsprechen, die verlangen, dass bestimmte Strings mit einem Großbuchstaben beginnen, zur Lesbarkeit oder aus stilistischen Vorlieben.

## Wie geht das:

Bash verfügt nicht über eine eingebaute Funktion speziell zum Kapitalisieren von Strings, aber Sie können diese Aufgabe mit Parametererweiterung oder externen Tools wie `awk` bewerkstelligen. Hier sind einige Wege, um einen String in Bash zu kapitalisieren:

**Verwendung der Parametererweiterung:**

Diese Methode manipuliert den String direkt in der Shell.

```bash
str="hello world"
capitalized="${str^}"
echo "$capitalized"
```
Ausgabe:
```
Hello world
```

**Verwendung von `awk`:**

`awk` ist ein leistungsstarkes Textverarbeitungstool, das auf den meisten Unix-ähnlichen Betriebssystemen verfügbar ist und zum Kapitalisieren von Strings genutzt werden kann.

```bash
str="hello world"
echo "$str" | awk '{print toupper(substr($0, 1, 1)) tolower(substr($0, 2))}'
```
Ausgabe:
```
Hello world
```

**Verwendung von `sed`:**

Für einen traditionelleren Ansatz kann `sed` verwendet werden, um den ersten Buchstaben eines Strings zu kapitalisieren. Es ist jedoch im Vergleich zu den vorherigen Methoden etwas komplexer.

```bash
str="hello world"
echo "$str" | sed 's/./\u&/'
```
Ausgabe:
```
Hello world
```

Diese Ausschnitte demonstrieren, wie man den ersten Buchstaben eines Strings in Bash kapitalisiert, und heben die Flexibilität der Shell-Skripterstellung bei der Textmanipulation hervor.
