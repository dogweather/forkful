---
title:    "C#: Einen String großschreiben"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

Warum: Warum jemand daran interessiert sein könnte, einen String zu kapitalisieren.

Wie: Eine Möglichkeit, einen String in C# zu kapitalisieren, ist die Verwendung der "ToUpper()" Methode. Hier ist ein Beispiel dafür:

```C# 
string text = "hallo welt!";
string capitalizedText = text.ToUpper();
```
Die Variable "capitalizedText" enthält nun den Wert "HALLO WELT!". Dies ist nützlich, wenn man sicherstellen möchte, dass Eingaben vom Benutzer einheitlich in Großbuchstaben behandelt werden.

Deep Dive: Wenn man genauer betrachtet, was bei der Verwendung der "ToUpper()" Methode passiert, sieht man, dass sie alle Buchstaben im String in ihre jeweiligen Großbuchstaben umwandelt. Dabei werden auch spezielle Zeichen wie Umlaute korrekt behandelt.

Für Entwickler, die mehr Kontrolle über die Groß- und Kleinschreibung haben möchten, bietet C# auch die Möglichkeit, den "CultureInfo" zu definieren, der bei der Umwandlung von Buchstaben berücksichtigt wird. Dadurch kann man zum Beispiel sicherstellen, dass auch spezifische deutsche Sonderzeichen korrekt umgewandelt werden.

See Also:

- MSDN-Dokumentation zur "ToUpper()" Methode (https://docs.microsoft.com/de-de/dotnet/api/system.string.toupper)
- Tutorial zur Kultur-spezifischen Groß- und Kleinschreibung in C# (https://www.c-sharpcorner.com/UploadFile/girish.nehte/working-with-string-comparisions-and-cultureinfo-in-C-Sharp/)