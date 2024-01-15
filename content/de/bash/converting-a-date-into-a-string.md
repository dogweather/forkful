---
title:                "Eine Datum in eine Zeichenfolge konvertieren."
html_title:           "Bash: Eine Datum in eine Zeichenfolge konvertieren."
simple_title:         "Eine Datum in eine Zeichenfolge konvertieren."
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum
Manchmal möchtest du vielleicht in deinem Bash-Skript ein Datum in eine Zeichenfolge umwandeln. Das kann nützlich sein, um es auf einer Benutzeroberfläche anzuzeigen oder in eine Datei zu schreiben.

## Wie geht das?
Grundsätzlich gibt es zwei Möglichkeiten, ein Datum in Bash in eine Zeichenfolge umzuwandeln.

### Methode 1: `date` Befehl
Der `date` Befehl ist der einfachste Weg, um ein Datum in eine Zeichenfolge umzuwandeln. Du kannst die Ausgabe des Befehls direkt in eine Variable speichern oder ihn in eine Zeichenfolgenersetzung einfügen.

```Bash
date_var=$(date + "%A, %d.%m.%Y")
echo $date_var
# Ausgabe: Donnerstag, 03.12.2020
```

### Methode 2: `printf` Befehl
Du kannst auch den `printf` Befehl verwenden, um ein Datum in eine Zeichenfolge umzuwandeln. Dazu musst du das Datum zunächst in die richtige Formatierung bringen und dann den `printf` Befehl verwenden.

```Bash
date_var=$(printf "%(%A, %d.%m.%Y)T" -1)
echo $date_var
# Ausgabe: Donnerstag, 03.12.2020
```

## Tiefer Einblick
Um das Datum in ein bestimmtes Format zu bringen, musst du die entsprechende Formatierungszeichenfolge verwenden. Eine Liste der verfügbaren Formatierungszeichenfolgen findest du in der [Bash-Dokumentation](https://www.gnu.org/software/bash/manual/html_node/Bash-Conditional-Expressions.html#Bash-Conditional-Expressions).

Wenn du das Datum in eine andere Sprache übersetzen möchtest, musst du die `LANG` Umgebungsvariable setzen. Zum Beispiel würde `LANG=de_DE.UTF-8` das Datum in deutscher Sprache ausgeben. Mehr Informationen dazu findest du in der [Bash-Referenz](https://tldp.org/LDP/abs/html/dateenvvar.html) Seite.

## Siehe auch
- [Bash-Dokumentation](https://www.gnu.org/software/bash/manual/)
- [Bash-Referenz](https://tldp.org/LDP/abs/html/)