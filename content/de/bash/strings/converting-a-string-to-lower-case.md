---
title:                "Umformung eines Strings in Kleinbuchstaben"
aliases:
- /de/bash/converting-a-string-to-lower-case/
date:                  2024-01-20T17:37:54.982811-07:00
model:                 gpt-4-1106-preview
simple_title:         "Umformung eines Strings in Kleinbuchstaben"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Umwandeln eines Strings in Kleinbuchstaben in Bash bedeutet, alle Großbuchstaben in einem Text in ihre entsprechenden Kleinbuchstaben zu ändern. Programmierer nutzen diese Umwandlung, um die Konsistenz bei Textvergleichen zu gewährleisten und Benutzereingaben zu standardisieren.

## Anleitung:

Ein Beispiel, wie man einen String in Bash mit eingebauten String-Operationen zu Kleinbuchstaben ändern kann:

```Bash
text="Heute Ist EIN schöner Tag!"
echo "${text,,}"
```

Erwartete Ausgabe:

```
heute ist ein schöner tag!
```

Verwendung der `tr` Kommandozeilen-Utility für denselben Effekt:

```Bash
echo "Heute Ist EIN schöner Tag!" | tr '[:upper:]' '[:lower:]'
```

Erneut ist die erwartete Ausgabe:

```
heute ist ein schöner tag!
```

## Tiefere Einblicke:

Früher, vor dem Aufkommen von Bash 4.0, gab es keine eingebaute Funktionalität, um Strings zu Kleinbuchstaben zu konvertieren. Man musste externe Kommandos wie `tr` oder `awk` nutzen. Seit Bash-Version 4.0 wurde die String-Manipulation jedoch stark erweitert, einschließlich obiger Methoden.

Alternativen:

- `awk '{print tolower($0)}'`
- `perl -ne 'print lc'`

Die Implementierung dieser Funktion in Bash ist besonders effizient, da sie direkt auf dem String operiert, ohne externe Prozesse zu starten.

## Siehe auch:

- Bash-Manual für String-Operationen: https://www.gnu.org/software/bash/manual/
- `tr` Kommandozeilen-Utility-Manual: https://man7.org/linux/man-pages/man1/tr.1.html
- AWK-Pattern-Scanning- und Processing-Sprachdokumentation: https://www.gnu.org/software/gawk/manual/gawk.html
- Perl-Programmierhandbuch: https://perldoc.perl.org/
