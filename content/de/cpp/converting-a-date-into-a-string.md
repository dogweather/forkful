---
title:                "Umwandlung eines Datums in einen String"
html_title:           "C++: Umwandlung eines Datums in einen String"
simple_title:         "Umwandlung eines Datums in einen String"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
"Datum in Zeichenfolge konvertieren" mag wie eine aufwendige Aufgabe klingen, aber es ist tatsächlich eine ziemlich häufige Aufgabe für Programmierer. Es bezieht sich einfach auf die Umwandlung eines Datums in eine menschenlesbare Form – ein String.
Warum tun wir das? Nun, manchmal müssen wir Daten in einem bestimmten Format ausgeben oder speichern, das vom Benutzer leichter zu lesen oder zu verarbeiten ist. Die Konvertierung eines Datums in eine Zeichenfolge ermöglicht es uns, dies zu tun und gleichzeitig die Flexibilität zu haben, das Format nach Bedarf anzupassen.

## Anleitung:
### Beispiel 1:
```C++
#include <iostream>
#include <string>
#include <ctime>

using namespace std;

int main()
{
    // Erstellen eines tm-Structs mit einem bestimmten Datum
    struct tm date = {0, 0, 0, 31, 12, 2021 - 1900};

    // Konvertieren in eine Zeichenfolge mit dem Format "31-12-2021"
    char str_date[11];
    strftime(str_date, 11, "%d-%m-%Y", &date);

    // Ausgabe des Ergebnisses
    cout << "Das Datum in Zeichenfolge konvertiert: " << str_date << endl;

    return 0;
}
```
Ausgabe:
```
Das Datum in Zeichenfolge konvertiert: 31-12-2021
```
Hier haben wir das Standard-C++-Daten- und Uhrzeit-Header-Datei <ctime> verwendet, um ein tm-Struct zu erstellen, das unser Datum enthält. Dann haben wir strftime verwendet, um das Datum in eine Zeichenfolge zu konvertieren, mit dem Format "%d-%m-%Y", das die Tag-Monat-Jahr-Reihenfolge verwendet und das Jahr in voller Länge anzeigt.

### Beispiel 2:
```C++
#include <iostream>
#include <string>
#include <chrono>
#include <iomanip>

using namespace std;

int main()
{
    // Erstellen eines currTime-Objekts, das die aktuelle Zeit enthält
    auto currTime = chrono::system_clock::now();

    // Konvertieren in eine Zeichenfolge mit dem Format "15-11-2019 20:20:00"
    time_t time = chrono::system_clock::to_time_t(currTime);
    char str_time[20];
    strftime(str_time, 20, "%d-%m-%Y %H:%M:%S", localtime(&time));

    // Ausgabe des Ergebnisses
    cout << "Das aktuelle Datum und Uhrzeit in Zeichenfolge konvertiert: " << str_time << endl;

    return 0;
}
```
Ausgabe:
```
Das aktuelle Datum und Uhrzeit in Zeichenfolge konvertiert: 15-11-2019 20:20:00
```
Hier haben wir <chrono> verwendet, um ein currTime-Objekt zu erstellen, das die aktuelle Zeit enthält. Dann haben wir strftime verwendet, um das Datum und die Uhrzeit in eine Zeichenfolge zu konvertieren, mit dem Format "%d-%m-%Y %H:%M:%S", das das Datum, die Stunden, Minuten und Sekunden anzeigt.

## Tiefere Einblicke:
Die Konvertierung eines Datums in eine Zeichenfolge mag zunächst trivial erscheinen, aber es gibt einige Aspekte, die beachtet werden müssen. Zum einen können wir verschiedene Formate verwenden, abhängig von unseren Anforderungen. Zum Beispiel könnte es hilfreich sein, einige der Grammatikfunktionen zu verwenden, die strftime anbietet, um die Ausgabe in der Sprache des Benutzers zu formatieren.

Außerdem ist es wichtig zu beachten, dass die Konvertierung umkehrbar sein muss – die Möglichkeit, eine Zeichenfolge in ein Datum zu konvertieren, ist genauso wichtig. Hierfür gibt es jedoch verschiedene Methoden und es ist wichtig, die entsprechende Methode zu wählen, die auf die Art und Weise angepasst ist, wie das Datum ursprünglich konvertiert wurde.

Schließlich gibt es noch andere Möglichkeiten, ein Datum in einer Zeichenfolge darzustellen, wie z.B. die Verwendung von Uhrzeit- und Datumsformaten, die internationalen Standards entsprechen, wie zum Beispiel ISO 8601.

## Siehe auch:
- [strftime Funktion](https://en.cppreference.com/w/cpp/chrono/strftime)
- [Zeichenfolgenkonvertierung in std::chrono](https://www.fluentcpp.com/2017/04/21/how-to-convert-std-time_t-to-std-chrono-system_clocktime_point/)
- [ISO 8601 Format](https://www.iso.org/iso-8601-date-and-time-format.html)