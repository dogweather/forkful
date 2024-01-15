---
title:                "Eine Zeichenfolge großschreiben"
html_title:           "C++: Eine Zeichenfolge großschreiben"
simple_title:         "Eine Zeichenfolge großschreiben"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum

Hast du jemals ein Programm geschrieben, das eine Benutzereingabe für einen Namen erhalten hat und diesen Namen dann in Großbuchstaben ausgegeben hat? In einem solchen Fall wäre es wichtig zu wissen, wie man in C++ eine Zeichenkette in Großbuchstaben konvertiert. Dieser Artikel wird dir zeigen, wie man das macht.

## Wie

Es gibt verschiedene Wege, um eine Zeichenkette in Großbuchstaben zu konvertieren, aber hier werden wir uns auf eine einfache Methode konzentrieren. Zuerst brauchen wir eine Zeichenkette, die wir in Großbuchstaben konvertieren möchten. Wir definieren also eine Variable vom Datentyp `string`, zum Beispiel `name` und weisen ihr einen Wert zu.

```C++
string name = "Max Mustermann";
```

Jetzt kommen wir zum eigentlichen Konvertieren. Dafür verwenden wir die Funktion `toupper` aus der Standardbibliothek `<cctype>`. Diese Funktion wandelt einen einzelnen Buchstaben in einen Großbuchstaben um. Aber um die gesamte Zeichenkette zu konvertieren, müssen wir sie in einer Schleife durchlaufen und jeden Buchstaben einzeln konvertieren.

```C++
for (int i = 0; i < name.length(); i++) {
  name[i] = toupper(name[i]);
}
```

Das ist so ziemlich alles. Wir haben jetzt die Zeichenkette `name` in Großbuchstaben konvertiert. Wir können das überprüfen, indem wir sie ausgeben:

```C++
cout << name << endl;
```

Die Ausgabe wird folgendermaßen aussehen: `MAX MUSTERMANN`

## Deep Dive

Wie genau funktioniert die `toupper` Funktion? Sie basiert auf dem ASCII-Code (American Standard Code for Information Interchange), der jedem Zeichen einen numerischen Wert zuordnet. Die Großbuchstaben und Kleinbuchstaben haben dabei eine unterschiedliche numerische Reihenfolge. Zum Beispiel hat der Großbuchstabe "A" den Wert 65, während der Kleinbuchstabe "a" den Wert 97 hat.

Die `toupper` Funktion prüft, ob der Wert eines Buchstabens im Bereich der Kleinbuchstaben liegt (97 bis 122). Wenn das der Fall ist, wird durch Subtraktion von 32 der entsprechende Großbuchstabenwert ermittelt (97 - 32 = 65). Andernfalls bleibt der Wert unverändert, sodass Großbuchstaben und Sonderzeichen nicht beeinflusst werden.

## Siehe Auch

- [TutorialsPoint - C++ - Konvertierung einer Zeichenkette in Großbuchstaben](https://www.tutorialspoint.com/cplusplus-program-to-convert-a-string-to-uppercase)
- [cpluplus.com - Referenz zu cctype](http://www.cplusplus.com/reference/cctype/toupper/)