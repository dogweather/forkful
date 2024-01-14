---
title:    "C++: Schreiben in den Standardfehler"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Warum

Das Schreiben von Fehlern auf die Standardfehlerausgabe kann ein nützliches Werkzeug sein, um Fehler in unserem C++ Code zu identifizieren und zu debuggen. Es ermöglicht uns, Fehlermeldungen im Konsolenfenster anzuzeigen, anstatt sie im regulären Ausgabestrom zu verstecken.

## Wie man

Um Fehler auf die Standardfehlerausgabe zu schreiben, müssen wir zuerst die Header-Datei `iostream` einbinden. Dann können wir die Funktion `std::cerr` verwenden, um unsere Fehlermeldung an die Konsole zu schreiben.

```C++
#include <iostream>
using namespace std;

int main() {
   cerr << "Dies ist ein Fehler!" << endl;
   return 0;
}
```

Die Ausgabe unseres Codes sieht folgendermaßen aus:

```
Dies ist ein Fehler!
```

Wir können auch Variablenwerte oder andere nützliche Informationen zu unseren Fehlermeldungen hinzufügen, indem wir sie einfach der `cerr` Funktion hinzufügen.

## Tiefentauchen

Die Standardfehlerausgabe ist besonders nützlich, wenn wir unsere Programme in der Konsole ausführen, da sie es uns ermöglicht, Fehler direkt im Konsolenfenster zu sehen, ohne auf die Ausgabedateien unseres Programms zugreifen zu müssen. Dies ist hilfreich bei der Fehlersuche und kann uns viel Zeit sparen.

Es ist auch wichtig zu beachten, dass die Standardfehlerausgabe nicht gepuffert wird, was bedeutet, dass die Fehlermeldungen in Echtzeit angezeigt werden. Dies kann bei der Fehlersuche hilfreich sein, da wir die Fehlermeldungen nicht manuell aus dem Puffer abrufen müssen.

## Siehe auch

- [C++ Fehlerbehandlung: Wann sind Ausnahmen besser als Fehlercodes?](https://www.programmieraufgaben.ch/aufgabe/c-fehlerbehandlung-wann-sind-ausnahmen-besser-als-fehlercodes/b32i4k)
- [So verwenden Sie `std::cout` und `std::cerr` in Lebensmitteln richtig](https://www.kompaktors.com/de/courses/cpp/lessons/how-to-use-std-cout-std-cerr)
- [Debugging-Techniken für C++ Anfänger](https://www.codiens.de/blog/2021/03/08/debugging-techniken-fuer-c-anfaenger/)