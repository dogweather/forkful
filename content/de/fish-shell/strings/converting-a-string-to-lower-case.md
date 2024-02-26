---
date: 2024-01-20 17:38:11.464868-07:00
description: "Das Umwandeln einer Zeichenkette in Kleinbuchstaben bedeutet, alle Gro\xDF\
  buchstaben in ihrem Inhalt in die entsprechenden Kleinbuchstaben zu \xFCberf\xFC\
  hren. Wir\u2026"
lastmod: '2024-02-25T18:49:51.345246-07:00'
model: gpt-4-1106-preview
summary: "Das Umwandeln einer Zeichenkette in Kleinbuchstaben bedeutet, alle Gro\xDF\
  buchstaben in ihrem Inhalt in die entsprechenden Kleinbuchstaben zu \xFCberf\xFC\
  hren. Wir\u2026"
title: Umformung eines Strings in Kleinbuchstaben
---

{{< edit_this_page >}}

## Was & Warum?
Das Umwandeln einer Zeichenkette in Kleinbuchstaben bedeutet, alle Großbuchstaben in ihrem Inhalt in die entsprechenden Kleinbuchstaben zu überführen. Wir tun dies, um bei Vergleichen oder der Datenverarbeitung Konsistenz zu gewährleisten und Schreibweise-bedingte Probleme zu vermeiden.

## Wie geht das:
```Fish Shell
# Umwandlung in Kleinbuchstaben mit `string tolower`
echo "Das Ist Ein BeISpiel" | string tolower
# Ausgabe: das ist ein beispiel
```

```Fish Shell
# Funktion in einer Schleife für mehrere Strings
for str in "Fisch" "Shell" "PrOgRaMmieRuNg"
    echo $str | string tolower
end
# Ausgabe:
# fisch
# shell
# programmierung
```

## Tiefere Einblicke:
Das Arbeiten mit Text in Skripten ist seit Anbeginn ein grundlegendes Element der Programmierung. Die Funktion `string tolower` in Fish Shell bietet eine out-of-the-box Lösung — ein luxuriöses Feature, das frühere Shells wie die Bourne Shell nicht hatten. Alternativ könnten Programme wie `awk` oder `tr` die Aufgabe in älteren Shells meistern:

```bash
echo "Etwas Älterer Weg" | tr '[:upper:]' '[:lower:]'
# Ausgabe: etwas älterer weg
```

Die `string` Befehlsfamilie in Fish Shell kam mit der Version 2.3.0 und vereinfachte Textoperationen enorm. Im Gegensatz zu POSIX-Shells brauchen wir keine externen Befehle für solche Elementaroperationen.

## Siehe auch:
- Offizielle Fish Shell Dokumentation zu String-Manipulationsbefehlen: [Fish Shell String](https://fishshell.com/docs/current/commands.html#string)
- Einen Vergleich von Textmanipulationswerkzeugen in verschiedenen Shells: [Popular Shell Comparison](https://hyperpolyglot.org/unix-shells)
