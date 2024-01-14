---
title:    "Arduino: Eine Zeichenkette großschreiben"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Warum

In der Arduino-Programmierung gibt es oft Szenarien, in denen es erforderlich ist, einen Teil eines Strings in Großbuchstaben umzuwandeln. Zum Beispiel könnte man einen Benutzernamen oder ein Passwort in Großbuchstaben anfordern, um sicherzustellen, dass die Eingabe korrekt ist. In diesem Blog-Beitrag werden wir uns ansehen, wie man in Arduino einen String in Großbuchstaben umwandelt.

## Wie man einen String in Großbuchstaben umwandelt

Das Konvertieren eines Strings in Großbuchstaben in Arduino ist relativ einfach. Dazu müssen wir die Funktion `toUpperCase()` verwenden, die in der Arduino-Referenzbibliothek enthalten ist. Schauen wir uns ein Beispiel an, wie man diese Funktion verwenden kann:

```Arduino
String name = "Arduino";
Serial.println(name.toUpperCase());
```

Die obige Codezeile wandelt den String `name` in Großbuchstaben um und gibt ihn über die serielle Schnittstelle aus. Das Ergebnis würde "ARDUINO" sein. Für eine dynamischere Umwandlung könnte man auch Benutzereingaben verwenden:

```Arduino
String input;
Serial.println("Bitte gib deinen Namen ein:");
while(Serial.available() == 0){
  // Warten auf Benutzereingabe
}

// Benutzereingabe lesen
input = Serial.readString();

Serial.println("Großbuchstaben-Version:");
Serial.println(input.toUpperCase());
```

In diesem Beispiel fordern wir den Benutzer auf, seinen Namen einzugeben, speichern die Eingabe in der Variable `input` und wandeln sie dann in Großbuchstaben um, bevor wir sie über die serielle Schnittstelle ausgeben.

## Tiefer Tauchen

Die Funktion `toUpperCase()` funktioniert, indem sie jeden Buchstaben im String in seinen entsprechenden Großbuchstaben umwandelt. Dies funktioniert unabhängig von der Sprache. Zum Beispiel würde der String "hello" sowohl in Englisch als auch in Deutsch in "HELLO" umgewandelt werden.

Es ist auch wichtig zu beachten, dass diese Funktion nur für Buchstaben funktioniert. Wenn man versucht, einen String mit Zahlen oder Sonderzeichen in Großbuchstaben zu konvertieren, werden diese Zeichen unverändert bleiben.

## Siehe auch

- [String-Klasse Referenz](https://www.arduino.cc/reference/de/language/variables/data-types/stringobject/)
- [String in Zeichenarray konvertieren](https://www.arduino.cc/reference/de/language/variables/data-types/string/functions/tochararray/)