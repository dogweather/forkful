---
title:                "Umwandlung eines Datums in einen String"
html_title:           "Fish Shell: Umwandlung eines Datums in einen String"
simple_title:         "Umwandlung eines Datums in einen String"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Konvertieren von Datumsangaben in Zeichenketten ist ein häufiges Problem, mit dem sich Programmierer konfrontiert sehen. Dabei geht es darum, ein Datum in ein menschenlesbares Format umzuwandeln, das leichter zu verarbeiten ist. Programmierer nutzen dies, um zum Beispiel Benutzerinformationen in einem übersichtlichen Format auszugeben oder um Vergleiche zwischen verschiedenen Datumsangaben anzustellen.

## Wie geht's?

Hier sind einige Beispiele dafür, wie man ein Datum in eine Zeichenkette mithilfe von Fish Shell konvertieren kann:

```
Fish Shell  set today (date +"%A, %B %e, %Y")
Monday, May 3, 2021
```

```
Fish Shell  set time (echo (date +"Today is %A at %l:%M %p"))
Today is Monday at 9:30 AM
```

```
Fish Shell  set date (string (date --date="1 month"))
Thu Jun 3 00:00:00 CEST 2021
```

## Tiefere Einblicke

Das Konvertieren von Datumsangaben in Zeichenketten ist keine neue Herausforderung. Es hat seinen Ursprung in der Notwendigkeit, Daten in einem bestimmten Format für die menschliche Lesbarkeit darzustellen. In Fish Shell gibt es auch andere Methoden, um Datumsangaben zu konvertieren, wie beispielsweise mithilfe des ```strftime``` Befehls.

## Siehe auch

Weitere Informationen zum Konvertieren von Datumsangaben in Zeichenketten mit Fish Shell können unter folgenden Links gefunden werden:

- [Fish Shell Dokumentation](https://fishshell.com/docs/current/index.html)
- [Tutorial zum Umgang mit Datum und Uhrzeit in Fish Shell](https://janikvonrotz.ch/2016/12/15/manipulate-time-and-date-in-fish-shell/)
- [Beispiele für die Verwendung von Datumsangaben in Fish Shell Scripts](https://gist.github.com/jasonwryan/3850615)