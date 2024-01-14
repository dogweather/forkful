---
title:    "Arduino: Schreiben auf die Standardfehlerausgabe"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Warum

In diesem Blog-Beitrag werden wir uns damit beschäftigen, wie man in Arduino Fehlermeldungen ausgeben kann. Das Schreiben von Meldungen an die Standardfehlerausgabe kann dabei helfen, Fehler in Ihrem Code zu finden und zu beheben. Es ist eine nützliche Technik, um Probleme während der Entwicklung zu erkennen.

## Wie geht es?

Um eine Fehlermeldung in Arduino auszugeben, verwenden Sie einfach die Funktion `Serial.println()`. Hier ist ein Beispielcode, der eine einfache Fehlermeldung ausgibt:

```
Arduino Serial.println("Fehler: Ungültige Spannungsquelle!");
```

Die Ausgabe dieses Codes wäre:

```
Fehler: Ungültige Spannungsquelle!
```

Ein weiteres Beispiel wäre die Ausgabe von Variablenwerten in einer Fehlermeldung. Nehmen wir an, Sie haben eine Variable `temperatur`, die den aktuellen Temperaturwert speichert. Sie können diesen Wert in der Fehlermeldung ausgeben, um zu sehen, ob Ihr Code richtig funktioniert:

```
Arduino Serial.println("Der aktuelle Temperaturwert beträgt: " + String(temperatur));
```

Die Ausgabe dieses Codes wäre ähnlich:

```
Der aktuelle Temperaturwert beträgt: 25 Grad Celsius
```

Es ist wichtig zu beachten, dass die Funktion `Serial.println()` nur Strings (Texte) ausgeben kann. Wenn Sie also eine variable Ausgabe benötigen, müssen Sie diese zuerst in einen String umwandeln, wie im obigen Beispiel mit der Funktion `String()`.

## Tiefer eintauchen

Die Funktion `Serial.println()` gibt die Fehlermeldung an die Standardfehlerausgabe aus. Sie können diese Meldungen dann mit einem Terminalprogramm (wie z.B. der Arduino IDE Serial Monitor) lesen. Sie können auch die Ausgabe an ein externes Gerät wie einen Computer oder ein Smartphone senden, indem Sie einen seriellen Port verwenden.

Um die serielle Kommunikation zu konfigurieren, können Sie die Funktion `Serial.begin()` verwenden. Diese Funktion muss in der `setup()` Funktion vor dem Aufruf von `Serial.println()` stehen.

Ein Beispiel für die Verwendung von `Serial.begin()`:

```
Arduino Serial.begin(9600);
```

Hier wird die Baudrate (Übertragungsrate) auf 9600 eingestellt, was der Standardwert für die serielle Kommunikation in Arduino ist. Sie müssen die Baudrate auf dem Empfängergerät (Computer, Smartphone, etc.) auf den gleichen Wert einstellen, um die Kommunikation erfolgreich zu machen.

## Siehe auch

- [Arduino Dokumentation über die Funktion `Serial.println()`](https://www.arduino.cc/reference/en/language/functions/communication/serial/println/)
- [Tutorial: Grundlegende serielle Eingabe/Ausgabe mit Arduino](https://www.arduino.cc/en/Tutorial/ReadASCIIString)
- [Videoanleitung zur Verwendung des Arduino Serial Monitors](https://www.youtube.com/watch?v=KcFmIkag_z8)