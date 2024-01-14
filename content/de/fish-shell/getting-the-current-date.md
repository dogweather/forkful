---
title:    "Fish Shell: Die aktuelle Datum abrufen"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Warum

Das Abrufen des aktuellen Datums in der Fish Shell mag auf den ersten Blick unbedeutend erscheinen, aber es kann sehr nützlich sein, wenn Sie Skripte schreiben, die mit zeitbasierten Aufgaben arbeiten. Zum Beispiel können Sie das aktuelle Datum verwenden, um Dateien oder Ordner mit dem entsprechenden Datum zu benennen oder um zu überprüfen, ob ein bestimmter Tag in der Zukunft liegt.

## Wie man es macht

Um das aktuelle Datum in der Fish Shell abzurufen, verwenden Sie die `date`-Funktion. Geben Sie einfach `date` in Ihrem Terminal ein und sie wird das aktuelle Datum und die aktuelle Uhrzeit ausgeben.

```Fish Shell
date
```
Output: Sam 02. Okt 2021 12:00:00 CEST

Sie können auch das Datum im gewünschten Format anzeigen lassen, indem Sie der `date` Funktion ein Formatmuster angeben. Zum Beispiel können Sie das Datum im ISO-Format `{YYYY}-{MM}-{DD}` anzeigen lassen, indem Sie `date {YYYY}-{MM}-{DD}` eingeben.

```Fish Shell
date {YYYY}-{MM}-{DD}
```
Output: 2021-10-02

Sie können auch das Datum in anderen Sprachen anzeigen lassen, indem Sie der `date`-Funktion die entsprechende Landeskennung geben. Zum Beispiel wird `date %d.%m.%Y` das Datum im Format `02.10.2021` in deutscher Sprache anzeigen.

```Fish Shell
date %d.%m.%Y
```
Output: 02.10.2021

## Tiefergehender Einblick

Die `date`-Funktion in der Fish Shell verwendet den Unix/Linux Befehl `date` unter der Haube, um das Datum abzurufen. Dadurch stehen Ihnen auch alle Optionen des `date` Befehls zur Verfügung, wenn Sie das Datum in einem bestimmten Format anzeigen lassen möchten. Verwenden Sie `man date` für weitere Informationen.

See Also
- [Fish Shell Dokumentation](https://fishshell.com/docs/current/index.html)
- [Unix/Linux Date Befehl](https://www.unix.com/man-page/linux/1/date/)