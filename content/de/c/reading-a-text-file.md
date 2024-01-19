---
title:                "Eine Textdatei lesen"
html_title:           "Bash: Eine Textdatei lesen"
simple_title:         "Eine Textdatei lesen"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Lesen einer Textdatei besteht darin, Daten aus einer externen Datei in unser Programm zu importieren. Wir als Programmierer tun dies häufig, um Benutzerdaten zu lesen oder anzuzeigen, oder um Eingaben für unsere Programme zu akzeptieren.

## So geht's:

Hier ist ein einfaches Beispiel für das Lesen einer Textdatei in C.

```C
#include <stdio.h>

int main() {
   char text[1000];
   FILE *file = fopen("Beispieltext.txt", "r");

   while (fgets(text, sizeof(text), file) != NULL) {
      printf("%s", text);
   }

   fclose(file);
   return 0;
}
```

Wenn Sie dieses Programm ausführen, sind die Inhalte von "Beispieltext.txt" auf Ihrem Bildschirm sichtbar.

## Vertiefung:

Textdateien wurden seit den Anfängen der Informatik zum Austausch von Daten verwendet. Es gibt andere Methoden, um Daten zu lesen, wie z.B. die Verwendung von Datenbanken oder Webdiensten, aber das Lesen von Textdateien bleibt eine einfache und effektive Methode.

Es gibt auch alternative Funktionen zum Lesen von Textdateien in C, einschließlich fscanf und fread. Beachten Sie, dass "fgets" nur bis zum nächsten Linefeed ("\\n") oder bis zur angegebenen Größe liest, welche zuerst auftritt, während "fread" genau die angegebene Anzahl von Bytes liest.

## Siehe auch:

Für weitere Informationen, schauen Sie sich folgende Quellen an:

1. [Lesen und Schreiben von Dateien in C](https://www.learn-c.org/de/Learn-C-files)
2. [fgets, fscanf und fread in C](http://www.cplusplus.com/reference/cstdio/)

Es ist immer gut, verschiedene Quellen zu überprüfen und zu sehen, wie die Praktiken variieren. Viel Spaß beim Codieren!