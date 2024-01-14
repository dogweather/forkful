---
title:                "C++: Eine Datumsumwandlung in einen String"
simple_title:         "Eine Datumsumwandlung in einen String"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum

Die Konvertierung eines Datums in einen String ist eine häufige Aufgabe in der Programmierung. Es kann wichtig sein, um Daten in bestimmten Formaten zu speichern oder benutzerfreundliche Informationen auszugeben. In diesem Blogbeitrag werden wir uns ansehen, wie man ein Datum in einen String umwandelt.

## How To

Um ein Datum in einen String umzuwandeln, benötigen wir die Funktion `strftime()` aus der C++ Standardbibliothek `<ctime>`. Diese Funktion erlaubt es uns, ein Datum in einem bestimmten Format auszugeben.

```C++
#include <ctime>
#include <iostream>

int main()
{
    // Aktuelles Datum erstellen
    time_t now = time(nullptr);

    // Format für den String definieren
    char buffer[80];
    strftime(buffer, 80, "Heute ist %A, der %d.%m.%Y", localtime(&now));

    // Ausgabe des String
    std::cout << buffer << std::endl;

    return 0;
}
```

Die Ausgabe dieses Codes würde folgendermaßen aussehen:

```
Heute ist Dienstag, der 19.10.2021
```

Hier haben wir das aktuelle Datum in einem benutzerfreundlichen Format ausgegeben. Der `%A` Platzhalter steht für den Wochentag (ausgeschrieben), `%d` für den Tag im Monat, `%m` für den Monat (als Zahl) und `%Y` für das Jahr.

Wenn man den ersten Teil des Codes verstehen möchte, sollte man sich mit der `time_t` Struktur auskennen, die die Anzahl an Sekunden seit dem 01.01.1970 speichert. In unserem Beispiel haben wir `time(nullptr)` verwendet, um die aktuelle Zeit als `time_t` Objekt zu erhalten. Dann haben wir die Funktion `localtime()` verwendet, um dieses Objekt in ein `tm` Objekt (Zeitstruktur) umzuwandeln, welches wir an die `strftime()` Funktion übergeben.

## Deep Dive

Die `strftime()` Funktion bietet viele weitere Möglichkeiten zur Formatierung von Datum und Uhrzeit. Hier sind einige nützliche Platzhalter:

- `%H`: Stunden im 24-Stunden-Format
- `%I`: Stunden im 12-Stunden-Format
- `%M`: Minuten
- `%S`: Sekunden
- `%p`: AM/PM Anzeige

Für eine vollständige Liste der verfügbaren Platzhalter und deren Verwendung empfehle ich die offizielle Dokumentation (Link in der "See Also" Sektion).

Außerdem unterstützt die `strftime()` Funktion auch die Ausgabe von Lokalisierungen. Das bedeutet, dass man das Datum und die Uhrzeit in der Sprache und im Format ausgeben kann, die im Betriebssystem des Benutzers eingestellt sind. Dazu muss man lediglich die Funktion `setlocale()` aus der C++ Standardbibliothek `<clocale>` verwenden. Allerdings sollte beachtet werden, dass diese Funktion nicht thread-sicher ist und daher in Multi-Thread Anwendungen mit Vorsicht verwendet werden muss.

## Siehe auch

- [Offizielle C++ Dokumentation für `strftime()`](https://en.cppreference.com/w/cpp/chrono/c/strftime)
- [Beispielcode für Datum- und Uhrzeitformatierung in verschiedenen Sprachen](https://www.ibsensoftware.com/Examples-Dates-Languages)
- [Einstellung der Lokalisierung in einer C++ Anwendung](https://docs.microsoft.com/en-us/cpp/c-runtime-library/language-strings-structures-and-localization?view=msvc-160#current_locale) (nur auf Englisch verfügbar)