---
title:    "C: Umwandeln eines Datums in einen String."
keywords: ["C"]
---

{{< edit_this_page >}}

# Warum

In der Informatik ist es oft notwendig, Daten in verschiedene Formate zu konvertieren. Eine häufige Aufgabe besteht darin, ein Datum in einen String umzuwandeln. In diesem Blog-Beitrag werden wir uns genauer ansehen, warum und wie man dies in der Programmiersprache C tun kann.

# Wie geht das?

Der erste Schritt ist die Verwendung der Funktion `strftime()` aus der Standardbibliothek `<time.h>`. Diese Funktion erlaubt es uns, ein gegebenes Datum mithilfe eines spezifischen Formats in einen String umzuwandeln. Hier ist ein Beispiel:

```C
#include <stdio.h>
#include <time.h>

int main() {
  time_t now = time(NULL);
  char str[50];

  strftime(str, sizeof(str), "%d-%m-%Y", localtime(&now));

  printf("Heute ist der %s", str);
  return 0;
}
```

In diesem Code nutzen wir `strftime()`, um das aktuelle Datum zu formatieren und in die Variable `str` zu speichern. Das Format `"%d-%m-%Y"` gibt das Datum im Format "TT-MM-JJJJ" aus. Die Funktion `localtime()` gibt einen Zeiger auf eine `struct tm` zurück, die das aktuelle Datum und die Uhrzeit enthält.

Wenn wir diesen Code ausführen, erhalten wir die Ausgabe "Heute ist der 25-04-2021". Natürlich gibt es viele andere Formatierungsoptionen, die je nach Bedarf genutzt werden können.

# Tiefer ins Thema einsteigen

Die Funktion `strftime()` kann nicht nur verwendet werden, um das aktuelle Datum zu formatieren, sondern auch um ein beliebiges Datum in einen String zu konvertieren. Dafür müssen wir die `struct tm` selbst initialisieren, anstatt die Funktion `localtime()` zu nutzen. Hier ist ein Beispiel, das das Datum des deutschen Einheitsfeiertags in einen String umwandelt:

```C
#include <stdio.h>
#include <time.h>

int main() {
  struct tm dTag = {
    .tm_mday = 3,
    .tm_mon = 10,
    .tm_year = 0
  };
  char str[50];

  strftime(str, sizeof(str), "%A, %d. %B %Y", &dTag);

  printf("Der deutsche Einheitsfeiertag ist dieses Jahr am %s", str);
  return 0;
}
```

Dieser Code gibt den String "Der deutsche Einheitsfeiertag ist dieses Jahr am Montag, 03. Oktober 1" aus. Beachten Sie, dass das Jahr in der `struct tm` als Anzahl von Jahren seit 1900 angegeben wird.

# Siehe auch

- [Dokumentation für die Funktion strftime()](https://www.cplusplus.com/reference/ctime/strftime/)
- [Eine Liste mit allen verfügbaren Formatierungsoptionen](https://strftime.org/) (in Englisch)
- [Weitere Informationen über die `struct tm`](https://www.cplusplus.com/reference/ctime/tm/) (in Englisch)