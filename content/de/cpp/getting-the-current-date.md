---
title:    "C++: Das aktuelle Datum erhalten"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

##Warum

Das Abrufen des aktuellen Datums ist eine häufige Anforderung in der Programmierung, da es bei der Verarbeitung von Daten oft wichtig ist, das Datum zu kennen, an dem diese erstellt oder geändert wurden. Es gibt verschiedene Methoden, um in C++ das aktuelle Datum abzurufen, aber in diesem Blogbeitrag werden wir uns auf die effizienteste Methode konzentrieren.

##Wie geht das?

Es gibt eine eingebaute Funktion in C++, die wir verwenden können, um das aktuelle Datum abzurufen. Diese Funktion heißt "std::chrono::system_clock::now()". Sie gibt uns eine instanziierte Variable des Typs "std::chrono::time_point", die das aktuelle Datum und die Uhrzeit enthält.

Hier ist ein Beispielcode, der das aktuelle Datum und die Uhrzeit in der Konsole ausgibt:

```C++
#include <iostream>
#include <chrono>
#include <ctime>

int main() {
  // Aktuelles Datum und Uhrzeit abrufen
  std::chrono::time_point<std::chrono::system_clock> now = std::chrono::system_clock::now();
  
  // Konvertieren in einen Zeitstempel
  std::time_t t = std::chrono::system_clock::to_time_t(now);
  
  // Zeitstempel in ein Datum und Uhrzeit Objekt konvertieren und ausgeben
  std::cout << std::ctime(&t) << std::endl;
  
  return 0;
}
```

Das Ergebnis sollte in etwa so aussehen:

```
Mo Dez 07 18:28:18 2020
```

Natürlich können wir auch das Ausgabeformat anpassen, je nachdem, wie wir das Datum und die Uhrzeit in unserem Programm verwenden möchten. Um zum Beispiel nur das Datum im Format "Tag.Monat.Jahr" anzuzeigen, könnten wir Folgendes tun:

```C++
#include <iostream>
#include <chrono>
#include <ctime>

int main() {
  // Aktuelles Datum und Uhrzeit abrufen
  std::chrono::time_point<std::chrono::system_clock> now = std::chrono::system_clock::now();
  
  // Konvertieren in einen Zeitstempel
  std::time_t t = std::chrono::system_clock::to_time_t(now);
  
  // Zeitstempel in ein Datum und Uhrzeit Objekt konvertieren
  std::tm* now_tm = std::localtime(&t);
  
  // Datum im gewünschten Format ausgeben
  std::cout << now_tm->tm_mday << "." << (now_tm->tm_mon + 1) << "." << (now_tm->tm_year + 1900) << std::endl;
  
  return 0;
}
```

Das Ergebnis wird dieses Mal nur das Datum anzeigen:

```
7.12.2020
```

##Tiefere Einblicke

Wenn wir uns genauer mit der internen Struktur und Funktionsweise der "std::chrono" Bibliothek beschäftigen möchten, gibt es einige wichtige Konzepte, die wir verstehen müssen:

- Uhr: Die Uhr ist das Zentrum der "std::chrono" Bibliothek. Sie repräsentiert die Zeit, die seit einem bestimmten Startpunkt (Epoch) vergangen ist.
- Uhrzeitpunkt: Dies ist eine instanziierte Variable des Typs "std::chrono::time_point". Sie enthält sowohl die Information über die Zeit seit dem Startpunkt als auch die Information über die verwendete Uhr.
- Dauer: Eine Dauer repräsentiert eine Zeitspanne in einem bestimmten Zeitintervall, zum Beispiel Sekunden, Minuten oder Stunden. Sie wird auch oft verwendet, um die Unterschiede zwischen zwei Uhrzeitpunkten zu messen.

Es gibt noch viele weitere Konzepte und Funktionen in der "std::chrono" Bibliothek, aber für das Abrufen des aktuellen Datums reicht es, die oben genannten Grundlagen zu verstehen.

##Weitere Informationen

Wenn du mehr über die "std::chrono" Bibliothek und das Arbeiten mit Datum und Uhrzeit in C++ erfahren möchtest, findest du hier einige nützliche Links:

- [cppreference.com - std::chrono](https://en.cppreference.com/w/cpp/chrono)
- [CppCon 2015: Howard Hinnant “A New Date And Time Library In C++"](https://www.youtube.com/watch?v=tzyGjOm8AKo)
- [Using std::chrono in C++](https://www.modernescpp.com/index.php/using-std-chrono