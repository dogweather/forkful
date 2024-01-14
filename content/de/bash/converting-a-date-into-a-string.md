---
title:    "Bash: Umwandlung eines Datums in einen String"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Konvertieren von Datum zu String ist eine grundlegende Fähigkeit für jeden Bash-Programmierer. Es ermöglicht die angemessene Darstellung von Datumselementen wie Tagen, Monaten und Jahren in einer leicht lesbaren und verständlichen Form.

## Wie geht man vor

Um ein Datum in einen String umzuwandeln, können wir die `date`-Funktion in Bash verwenden. Diese Funktion kann mit verschiedenen Parametern aufgerufen werden, die das Format der Ausgabe bestimmen. Hier ist ein Beispielcode:

```Bash
date +"%d.%m.%Y" # Ausgabe: 27.09.2021
```

Im obigen Beispiel wird das Datum im Format von Tag.Monat.Jahr ausgegeben. Hier sind einige gängige Parameter, die verwendet werden können, um das Ausgabeformat zu ändern:

- `%d`: Tag (zweistellig)
- `%m`: Monat (zweistellig)
- `%Y`: Jahr (vierstellig)
- `%y`: Jahr (zweistellig)
- `%H`: Stunde (zweistellig, 24-Stunden Format)
- `%I`: Stunde (zweistellig, 12-Stunden Format)
- `%M`: Minute (zweistellig)
- `%S`: Sekunde (zweistellig)

Es gibt viele weitere Parameter, die verwendet werden können, je nachdem, wie genau das Datum oder die Uhrzeit angezeigt werden sollen. Es ist ratsam, die offizielle Dokumentation der `date`-Funktion zu konsultieren, um alle verfügbaren Optionen zu erfahren.

Eine weitere Möglichkeit, ein Datum in einen String umzuwandeln, ist die Verwendung der `strftime`-Funktion, die eine ähnliche Syntax verwendet wie die `date`-Funktion, aber mehr Flexibilität bietet. Hier ist ein Beispielcode:

```Bash
STRFTIME_FORMAT="%A, %B %d, %Y" # Einstellen des gewünschten Ausgabeformats
date +"$STRFTIME_FORMAT" # Ausgabe: Montag, September 27, 2021
```

Es gibt auch die Möglichkeit, mit der `date`-Funktion eine Eingabe von Benutzern zu verarbeiten und in ein Datum umzuwandeln. Hier ist ein Beispielcode:

```Bash
read -p "Geben Sie ein Datum im Format Jahr-Monat-Tag ein: " input_date
date -d "$input_date" +"%d.%m.%Y" # Eingabe: 2021-09-27, Ausgabe: 27.09.2021
```

## Tiefere Einblicke

Das Konvertieren von Datum zu String ermöglicht es uns, das Datum in verschiedenen Formaten darzustellen, je nach den Anforderungen unserer Anwendung. Es kann auch für das Vergleichen von Daten und das Sortieren von Datensätzen verwendet werden. Es ist wichtig zu beachten, dass die `date`-Funktion eine sehr leistungsstarke Funktion ist, die es uns ermöglicht, Datumsoperationen in Bash durchzuführen, und es lohnt sich, mehr darüber zu lernen.

## Siehe auch

- [Offizielle `date`-Dokumentation](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [`strftime`-Referenz](https://linux.die.net/man/3/strftime)