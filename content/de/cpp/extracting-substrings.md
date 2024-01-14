---
title:                "C++: Substrings extrahieren"
simple_title:         "Substrings extrahieren"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

Warum: Warum sollte man sich überhaupt mit der Extrahierung von Teilstrings beschäftigen? Die Antwort ist einfach: Teilstrings werden in vielen Programmieranwendungen benötigt, sei es für die Verarbeitung von Benutzereingaben oder für die Manipulation von Texten.

Wie geht man vor: Man kann Teilstrings auf verschiedene Arten extrahieren, aber hier werden wir uns auf die Verwendung der in C++ eingebauten Funktion `substr()` konzentrieren. Diese Funktion nimmt zwei Parameter an: den Startindex und die Länge des Teilstrings. Hier ist ein Beispiel, wie man `substr()` benutzen kann, um "Hallo" aus einem String "Hallo Welt!" zu extrahieren:

```
C++ string str = "Hallo Welt!";
string sub = str.substr(0, 5);
cout << sub << endl;
```

Dieses Beispiel gibt "Hallo" als Output aus. Der Startindex ist 0, da der erste Buchstabe des Teilstrings "H" an der Position 0 im ursprünglichen String steht. Die Länge des Teilstrings ist 5, da "Hallo" aus 5 Buchstaben besteht. Man kann auch `substr()` verwenden, um Teilstrings von hinten zu extrahieren, indem man einen negativen Startindex angibt. Zum Beispiel würde `str.substr(-6, 3);` den Teilstring "Welt" aus unserem ursprünglichen String ausgeben.

Tiefere Einblicke: Anstatt die Startposition und Länge manuell einzugeben, kann man auch `find()` verwenden, um die Position eines bestimmten Zeichens oder Teilstrings im String zu finden und diese Informationen dann an `substr()` zu übergeben. Zum Beispiel kann man mit `find()` die Position des Leerzeichens in unserem String "Hallo Welt!" finden und diese Position als Startindex in `substr()` verwenden. Das würde folgendermaßen aussehen: `substr(str.find(" "), 6);`. Dies würde den Teilstring "Welt!" aus unserem ursprünglichen String ausgeben.

Auch ist es möglich, `substr()` mehrfach zu verwenden, um aus einem String mehrere Teilstrings zu extrahieren. Dafür muss man nur die Startposition und Länge jedes Teilstrings entsprechend angeben.

Siehe auch:
-https://www.cplusplus.com/reference/string/string/substr/
-https://www.geeksforgeeks.org/c-strings-library-c-str/
-https://en.cppreference.com/w/cpp/string/basic_string/substr