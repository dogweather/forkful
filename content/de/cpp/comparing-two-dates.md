---
title:    "C++: Vergleich von zwei Daten"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Warum

Das Vergleichen von zwei Daten ist eines der grundlegenden Konzepte in der Programmierung. Es hilft uns, Entscheidungen in unseren Programmen zu treffen und sicherzustellen, dass unsere Anwendungen korrekt und effizient funktionieren. Wenn Sie ein Programmierer sind, ist es wichtig, die Grundlagen des Vergleichs von Daten zu verstehen, um erfolgreich Code zu schreiben.

## Wie man es macht

Um zwei Daten in C++ zu vergleichen, können wir den Vergleichsoperator '==' verwenden, der wahr zurückgibt, wenn die zwei Ausdrücke gleich sind und falsch, wenn sie nicht gleich sind. Schauen wir uns ein Beispiel an:

```C++
int date1 = 20210221;
int date2 = 20210305;
if (date1 == date2) {
    cout << "Die beiden Daten sind gleich!" << endl;
} else {
    cout << "Die Daten sind unterschiedlich!" << endl;
}
```

Die Ausgabe dieses Codes wäre: "Die Daten sind unterschiedlich!", da die beiden Daten nicht gleich sind.

Wir können auch den größer-als ('>') und kleiner-als ('<') Operator verwenden, um zu überprüfen, ob eine Daten größer oder kleiner als eine andere ist. Schauen wir uns ein weiteres Beispiel an:

```C++
int date3 = 20210305;
if (date3 > date2) {
    cout << "Das Datum 20210305 ist größer als 20210221!" << endl;
} else {
    cout << "Das Datum 20210305 ist kleiner als 20210221!" << endl;
}
```

Die Ausgabe dieses Codes wäre: "Das Datum 20210305 is kleiner als 20210221!", da 20210305 ursprünglich kleiner als 20211222 war, aber nachdem 20211222 auf 20210305 zugeordnet wurde, ist es nun größer.

## Tiefer Einblick

Es ist wichtig zu erwähnen, dass wir in C++ auch die Datenstruktur "struct tm" verwenden können, um komplexe Daten wie Datum und Uhrzeit darzustellen und zu vergleichen. Diese Datenstruktur bietet zusätzliche Funktionen wie das Überprüfen des Wochentages, das Formatieren der Ausgabe und das Berechnen von Zeitdifferenzen. Wenn Sie tiefer in das Thema einsteigen möchten, können Sie sich die offizielle Dokumentation von C++ zu "struct tm" ansehen.

## Siehe auch

- [Die Grundlagen von Strukturen in C++](https://www.cplusplus.com/doc/tutorial/structures/)
- [C++ Vergleichsoperatoren](https://www.tutorialspoint.com/cplusplus-relational-operators)
- [Die offizielle Dokumentation von C++ zu "struct tm"](https://www.cplusplus.com/reference/ctime/tm/)