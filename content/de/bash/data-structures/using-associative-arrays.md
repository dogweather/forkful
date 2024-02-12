---
title:                "Verwendung von assoziativen Arrays"
date:                  2024-01-30T19:09:52.688770-07:00
model:                 gpt-4-0125-preview
simple_title:         "Verwendung von assoziativen Arrays"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Assoziative Arrays sind wie aufgeladene Arrays, die es Ihnen erlauben, Strings als Indizes anstelle von nur Ganzzahlen zu verwenden. Programmierer nutzen sie für komplexere Datenstrukturen, was es einfacher macht, Daten zu handhaben, die nicht sauber in eine sequenzielle Liste passen.

## Wie:

Zuerst deklarieren Sie ein assoziatives Array in Bash:

```Bash
declare -A my_array
```

Dann können Sie beginnen, es mit Werten zu füllen, indem Sie Strings als Schlüssel verwenden:

```Bash
my_array["name"]="Linux Journal"
my_array["topic"]="Programmierung"
```

Um auf ein Element zuzugreifen, verwenden Sie dessen Schlüssel:

```Bash
echo ${my_array["name"]}  # Gibt aus: Linux Journal
```

Das Iterieren über Schlüssel und Werte ist ebenfalls unkompliziert:

```Bash
for key in "${!my_array[@]}"; do
    echo "$key: ${my_array[$key]}"
done
```

Eine Beispiel-Ausgabe könnte so aussehen:

```
name: Linux Journal
topic: Programmierung
```

Um Elemente hinzuzufügen oder zu modifizieren, weisen Sie einfach einem Schlüssel einen Wert zu, ähnlich wie bei der anfänglichen Befüllung:

```Bash
my_array["Leser"]="Ihr"
```

Und um ein Element zu entfernen, verwenden Sie `unset`:

```Bash
unset my_array["Thema"]
```

## Tiefergehende Betrachtung

Assoziative Arrays wurden in Bash-Version 4.0 eingeführt und sind somit eine relativ neue Ergänzung der Sprache. Vor ihrer Einführung war das Handhaben von Arrays mit Nicht-Ganzzahlen-Indizes umständlich, oft waren Umwege oder externe Werkzeuge wie `awk` oder `sed` erforderlich.

Unter der Haube implementiert Bash assoziative Arrays mit Hash-Tabellen. Diese Implementierung erlaubt eine effiziente Schlüsselsuche, die unabhängig von der Größe des Arrays ziemlich konstant bleibt, ein kritisches Merkmal für die Leistung bei der Skriptausführung.

Während assoziative Arrays in Bash viel Macht und Flexibilität für das Shell-Skripting bringen, kommen sie mit ihrem eigenen Satz von Einschränkungen, wie zum Beispiel etwas umständlicher zu sein im Vergleich mit Arrays in höheren Sprachen wie Python oder JavaScript. Für komplexe Datenmanipulationsaufgaben könnte es immer noch erwägenswert sein, externe Werkzeuge oder Sprachen zu betrachten, die besser für den Job geeignet sind.

Jedoch bieten assoziative Arrays für viele typische Skripting-Aufgaben ein wertvolles Werkzeug im Toolkit des Bash-Programmierers, indem sie lesbarere und wartbarere Skripte ermöglichen, da sie die Verwendung von bedeutungsvollen String-Schlüsseln anstelle von numerischen Indizes erlauben.
