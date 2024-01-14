---
title:    "Bash: Das aktuelle Datum erhalten"
keywords: ["Bash"]
---

{{< edit_this_page >}}

#Warum

In diesem Blogbeitrag geht es um das Abrufen des aktuellen Datums in Bash-Programmierung. Dies ist eine nützliche Fähigkeit, die beim Schreiben von Skripten und Automatisierungsaufgaben hilfreich sein kann.

##Wie man das aktuelle Datum abruft

Um das aktuelle Datum in Bash abzurufen, können wir die `date`-Funktion verwenden. Hier ist ein Beispielcode:

```Bash
date
```

Dieser Befehl gibt das aktuelle Datum und die Uhrzeit im Standardformat aus. Zum Beispiel:

```
Fri Jun 4 14:31:25 CEST 2021
```

Wir können auch das Format des Datums mithilfe von Optionen ändern. Zum Beispiel, um nur das Datum im Format YYYY-MM-DD anzuzeigen, können wir Folgendes verwenden:

```Bash
date +%Y-%m-%d
```

Das Ergebnis wäre dann:

```
2021-06-04
```

Es gibt viele verschiedene Optionen, die wir der `date`-Funktion übergeben können, um das Datum in verschiedenen Formaten anzuzeigen. Eine vollständige Liste aller Optionen finden Sie in der `man`-Seite der `date`-Funktion.

##Tiefere Einblicke

Das Abrufen des aktuellen Datums ist eine grundlegende Fähigkeit in der Bash-Programmierung, aber es gibt viele weitere Möglichkeiten, mit Datum und Zeit zu arbeiten. Eine davon ist das Vergleichen von Datumsangaben, um zum Beispiel festzustellen, ob ein bestimmtes Datum in der Zukunft liegt oder nicht. Dies kann besonders nützlich sein, wenn Sie automatisierte Aufgaben erstellen, die basierend auf dem aktuellen Datum ausgeführt werden sollen.

Eine weitere nützliche Fähigkeit ist das Ändern von Datumsangaben. Zum Beispiel können wir mithilfe der `date`-Funktion ein bestimmtes Datum in der Zukunft oder Vergangenheit berechnen, indem wir eine Anzahl von Tagen oder sogar die Anzahl von Sekunden angeben.

Die Bash-Programmierung bietet viele weitere Möglichkeiten, mit Datum und Zeit umzugehen, die über den Umfang dieses Blogbeitrags hinausgehen. Es empfiehlt sich, weitere Ressourcen und Tutorials zu diesem Thema zu lesen, um Ihr Verständnis zu vertiefen.

##Siehe auch

- [Bash-Dokumentation zu Datum und Uhrzeit](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameters.html)
- [Liste aller `date`-Optionen](https://www.cyberciti.biz/faq/linux-unix-formatting-dates-for-display/)
- [Tutorial zum Vergleichen von Datumsangaben in Bash](https://www.lifewire.com/comparing-dates-in-bash-script-2200570)
- [Tutorial zur Manipulation von Datumsangaben in Bash](https://www.unixtutorial.org/manipulating-date-and-time-with-date-in-linux-bash/)