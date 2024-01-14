---
title:    "C++: Unterzeichenketten extrahieren"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum?

Das Extrahieren von Teilstrings ist eine nützliche Fähigkeit, die in der Programmierung oft benötigt wird. Es ermöglicht uns, nur die Teile eines Textes zu isolieren, die wir benötigen, anstatt den gesamten String zu durchsuchen.

## Wie geht das?

Um Teilstrings in C++ zu extrahieren, können wir die `substr()` Funktion verwenden. Diese Funktion nimmt zwei Parameter an: den Startindex und die Länge des gewünschten Teilstrings. Hier ist ein Beispielcode, der die `substr()` Funktion verwendet:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
	// Definieren des Ausgangsstrings
	string satz = "Das Wetter ist heute schön";

	// Extrahieren des zweiten Teilstrings "Wetter"
	string wetter = satz.substr(4, 6);

	// Ausgabe des Teilstrings
	cout << "Teilstring: " << wetter << endl;

	// Extrahieren des dritten Teilstrings "ist"
	string ist = satz.substr(11, 3);

	// Ausgabe des Teilstrings
	cout << "Teilstring: " << ist << endl;

 	return 0;
}
```

Der Output dieses Codes wäre:

```
Teilstring: Wetter
Teilstring: ist 
```

## Tiefergehende Informationen

Bei der Verwendung der `substr()` Funktion müssen wir darauf achten, dass der Startindex innerhalb der Länge des ursprünglichen Strings liegt, sonst wird ein Fehler auftreten. Außerdem wird die Länge des Teilstrings nicht automatisch an das Ende des ursprünglichen Strings angepasst, daher müssen wir sicherstellen, dass die angegebene Länge nicht über die Länge des ursprünglichen Strings hinausgeht.

## Siehe auch

- [C++ - Strings](https://www.cplusplus.com/doc/tutorial/ntcs/)
- [C++ - substr() Funktion](http://www.cplusplus.com/reference/string/string/substr/)