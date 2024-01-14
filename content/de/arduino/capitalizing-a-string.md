---
title:    "Arduino: Eine Zeichenkette großschreiben"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum:

Sie haben vielleicht schon einmal von der Funktion "toUpper()" in Arduino gehört und sich gefragt, wofür sie gut ist. Die Antwort ist einfach: "toUpper()" wandelt einen String in Großbuchstaben um. Aber warum sollte man das überhaupt tun wollen?

Es gibt viele Anwendungsbereiche, in denen es wichtig ist, dass ein String in Großbuchstaben ist. Zum Beispiel kann es bei der Verarbeitung von Benutzereingaben oder beim Vergleich von Strings hilfreich sein, wenn alle Buchstaben in der gleichen Größe sind. Auch bei der Ausgabe von Text auf einem Display kann es optisch ansprechender sein, wenn alles in Großbuchstaben geschrieben ist.

## Wie geht das?

Zunächst müssen wir einen String definieren, den wir in Großbuchstaben umwandeln möchten. Dazu können wir die Variable "text" erstellen und einen beliebigen Text zuweisen.

```Arduino
String text = "Hallo Arduino Freunde!";
```

Dann verwenden wir die Funktion "toUpper()" und weisen ihr den String "text" zu.

```Arduino
text = text.toUpper();
```

Und voilà, unser String wird automatisch in Großbuchstaben umgewandelt. Um dies in der seriellen Monitor auszugeben, können wir einfach folgenden Code verwenden:

```Arduino
Serial.println(text);
```

Die Ausgabe wird dann "HALLO ARDUINO FREUNDE!" lauten.

## Tiefer gehende Informationen:

Wenn Sie sich für die technischen Details interessieren, können wir einen kurzen Blick auf die Funktion "toUpper()" werfen. Diese Funktion wird in der Arduino String Klasse implementiert und verwendet die ASCII-Tabelle, um jeden Buchstaben in seinen Großbuchstaben zu transformieren.

Es ist wichtig zu beachten, dass die Funktion nur Buchstaben umwandelt. Sonderzeichen, Zahlen oder Leerzeichen bleiben unverändert. Außerdem wird der ursprüngliche String nicht verändert, sondern es wird ein neuer String mit den Großbuchstaben erstellt.

## Siehe auch:

Weitere Informationen und Beispiele zur Verwendung von "toUpper()" finden Sie in der offiziellen Arduino Dokumentation:
- https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/toupper/
- https://arduino-projekte.webnode.at/l/string-gros-schreiben-mit-arduino/
- https://wolles-elektronikkiste.de/string-in-grossbuchstaben/

Wir hoffen, dass dieser Artikel Ihnen geholfen hat, die Verwendung von "toUpper()" besser zu verstehen. Viel Spaß beim Experimentieren mit Strings und viel Erfolg bei Ihren zukünftigen Arduino Projekten!