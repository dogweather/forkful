---
title:                "C++: Ein Datum in einen String umwandeln."
programming_language: "C++"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum
Das Konvertieren eines Datums in einen String kann hilfreich sein, wenn man beispielsweise Daten in einer lesbaren und benutzerfreundlichen Form präsentieren möchte. Außerdem kann es auch zur Umwandlung von Datumsangaben in verschiedenen Formaten nützlich sein.

## Wie geht das?
Um ein Datum in einen String umzuwandeln, verwenden wir in C++ die Funktion `strftime()`. Diese Funktion erwartet drei Parameter: einen Zeiger auf einen char-Array, der das Ergebnis der Umwandlung speichert, ein Formatierungsstring und eine Struktur, die das Datum enthält.

```C++
#include <iostream>
#include <ctime>

using namespace std;

int main() {
  // Datum erstellen
  time_t now = time(0);
  tm *date = localtime(&now);

  // Char-Array erstellen, um das Datum zu speichern
  char strDate[100];

  // Datum in String umwandeln
  strftime(strDate, 100, "Heute ist %d.%m.%Y", date);

  // String ausgeben
  cout << strDate << endl;

  return 0;
}

```

Die Ausgabe dieses Codes wäre: "Heute ist 28.06.2021".

## Tiefere Einblicke
Die Funktion `strftime()` basiert auf einer älteren Funktion namens `asctime()`, die jedoch nur englische Monatsnamen verwendet. `strftime()` hingegen erlaubt es uns, beliebige Sprachen für die Monatsnamen zu verwenden, indem wir ein spezielles Formatierungssymbol `%Z` verwenden und die gewünschten Monatsnamen als Parameter übergeben.

```C++
// Setzen der französischen Monatsnamen
setlocale(LC_TIME, "fr_FR.utf8");
strftime(strDate, 100, "Aujourd'hui c'est le %d %B %Y", date);

// Ausgabe: Aujourd'hui c'est le 28 juin 2021

```

Weitere Informationen und Formatierungsmöglichkeiten für die Funktion `strftime()` finden Sie in der offiziellen C++ Dokumentation.

## Siehe auch
- [C++ strftime Dokumentation](https://www.cplusplus.com/reference/ctime/strftime/)
- [C++ asctime Dokumentation](https://www.cplusplus.com/reference/ctime/asctime/)