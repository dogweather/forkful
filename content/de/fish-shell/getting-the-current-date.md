---
title:                "Fish Shell: Das aktuelle Datum erhalten"
simple_title:         "Das aktuelle Datum erhalten"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum

Es mag zunächst ungewöhnlich erscheinen, aber es gibt tatsächlich viele Gründe, warum jemand das aktuelle Datum in sein Fish Shell-Programm einbinden möchte. Vielleicht möchten Sie eine automatische Datumsangabe in Ihre Shell-Befehle einfügen, ein Skript erstellen, das basierend auf dem aktuellen Datum verschiedene Aktionen ausführt, oder einfach nur aus Neugier wissen, welcher Tag heute ist.

## Anleitung

Um das aktuelle Datum im Fish Shell abzurufen, können Sie das Befehlszeilenwerkzeug 'date' verwenden. Dieser Befehl gibt das Datum im Format "Tag Monat Jahr" aus. Beispielsweise würde der Befehl "date" heute (24. September 2021) folgendes zurückgeben:

```Fish Shell
Fr 24 Sep 2021
```

Wenn Sie das Datum in einem anderen Format erhalten möchten, können Sie die Option '-s' verwenden, gefolgt von dem von Ihnen gewünschten Format. Beispielsweise würde der Befehl "date -s "%d.%m.%Y"" heute (24. September 2021) folgendes zurückgeben:

```Fish Shell
24.09.2021
```

Es gibt viele verschiedene Optionen für das Datum, die Sie verwenden können, abhängig von Ihrem gewünschten Format. Für eine vollständige Liste der verfügbaren Optionen können Sie die Manpage des 'date'-Befehls aufrufen.

## Tiefer Einblick

Bei näherer Betrachtung ist das Abrufen des aktuellen Datums in Fish Shell etwas komplexer als es auf den ersten Blick erscheint. Tatsächlich ist das 'date'-Befehlszeilenwerkzeug Teil des Kerns des Betriebssystems und wird standardmäßig in den meisten Linux-Distributionen verwendet. Es verwendet die Systemzeit des Computers und formatiert sie entsprechend den von Ihnen angegebenen Optionen.

Die Verwendung von 'date' zeigt auch die unglaubliche Flexibilität und Anpassungsfähigkeit von Fish Shell. Sie können es verwenden, um das Datum auf verschiedene Arten zu formatieren und in Ihre Skripte oder Befehle einbinden, um ihnen zusätzliche Funktionen zu verleihen.

## Siehe auch

- [Manpage des 'date'-Befehls](https://manpages.debian.org/latest/coreutils/date.1.en.html)
- [Fish Shell-Dokumentation](https://fishshell.com/docs/current/index.html)
- [Linuxize-Artikel über das Abrufen des aktuellen Datums in der Shell](https://linuxize.com/post/bash-get-current-date/)