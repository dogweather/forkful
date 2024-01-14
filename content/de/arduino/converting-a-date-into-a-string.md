---
title:    "Arduino: Eine Datumsangabe in einen String umwandeln"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Warum

Die Umwandlung eines Datums in einen String ist eine häufige Aufgabe bei der Arduino-Programmierung. Dies ist besonders nützlich, wenn man das Datum auf einem Display oder in einer seriellen Schnittstelle ausgeben möchte. In diesem Blogbeitrag werden wir uns anschauen, wie man diese Konvertierung mit Arduino durchführen kann.

# Wie geht das?

Es gibt verschiedene Möglichkeiten, ein Datum in einen String umzuwandeln, aber eine der einfachsten Methoden ist die Verwendung der Funktion `sprintf()`. Diese Funktion ermöglicht es uns, eine Zeichenfolge formatiert zu erstellen, die das Datum enthält. 

```Arduino
#include <TimeLib.h> 

void setup(){
  Serial.begin(9600);
  setTime(17,15,00,3,9,2021); //Stellt das Datum auf 17:15, 3. September 2021
}

void loop(){
  char dateStr[11];
  sprintf(dateStr, "%02d.%02d.%04d", day(), month(), year()); //Speichert das Datum im Format TT.MM.JJJJ in dateStr
  Serial.println(dateStr); //Gibt das Datum im seriellen Monitor aus
  delay(1000);
}
```

Wenn wir den obigen Code ausführen, sollten wir im seriellen Monitor das Datum 03.09.2021 sehen. Hier verwenden wir das Format `" %02d.%02d.%04d"`, um das Datum in der Form TT.MM.JJJJ zu erhalten. Wir könnten auch andere Formate wie "TT-MM-JJJJ" oder "JJJJ/MM/DD" verwenden, je nachdem, welche Art von String wir benötigen.

# Tiefer gehend

Um das Datum in einen String umzuwandeln, muss das `TimeLib`-Bibliothek in unserem Sketch hinzugefügt werden. Diese Bibliothek enthält Funktionen, die es uns ermöglichen, Werte wie Stunden, Minuten, Tage, Monate und Jahre zu speichern.

Die Funktion `sprintf()` wird normalerweise verwendet, um ein oder mehrere Variablen in eine Zeichenfolge zu formatieren, indem sie einen Platzhalter für jede Variable verwendet. In unserem Beispiel haben wir `%02d` verwendet, um eine Ganzzahl mit zwei Ziffern darzustellen. Wenn der Tag z.B. einstellig ist, fügt `sprintf()` eine führende Null hinzu, um immer zwei Ziffern anzuzeigen.

Es gibt auch andere Bibliotheken und Funktionen, die verwendet werden können, um ein Datum in einen String umzuwandeln. Es ist wichtig, sicherzustellen, dass das verwendete Format mit den Anforderungen unserer Anwendung übereinstimmt.

# Siehe auch

- [Tutorial: Set Up and Use sprintf() on Arduino](https://www.arduino.cc/reference/en/language/functions/character-functions/sprintf/)
- [Arduino Date and Time Library](https://github.com/PaulStoffregen/Time)
- [Forum: Formatting dates](https://forum.arduino.cc/index.php?topic=620505.0)