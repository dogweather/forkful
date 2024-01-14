---
title:    "Arduino: Schreiben auf Standardfehler"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Warum 

Wenn du anfangen möchtest, mit Arduino zu programmieren, wirst du oft auf die Funktion `Serial.println()` stoßen. Aber was ist mit `Serial.print()` und `Serial.write()`? In diesem Blogbeitrag werden wir uns auf die letzte Funktion konzentrieren und warum es wichtig ist, sie zu verwenden.

# Wie man es macht

Um mit Arduino auf den Standardfehler zu schreiben, musst du zunächst die serielle Schnittstelle initialisieren. Dazu verwendest du die Funktion `begin()` im Setup-Teil deines Codes:

```Arduino
void setup() {
  Serial.begin(9600);
}
```

Als nächstes ist es wichtig, die Fehlermeldung zu definieren, die du ausgeben möchtest. Dazu verwendest du den Befehl `sprintf()`:

```Arduino
char error[50];
sprintf(error, "Es ist ein Fehler aufgetreten: %d", errorCode);
```

Schließlich verwendest du die Funktion `Serial.write()` um die Fehlermeldung auf den Standardfehler zu schreiben:

```Arduino
Serial.write(error);
```

# Tiefer eintauchen

Die Verwendung von `Serial.write()` ermöglicht es uns, Fehlermeldungen auf den Standardfehler zu schreiben, anstatt sie nur auf dem seriellen Monitor auszugeben. Dies ist besonders nützlich, wenn unser Code mit anderen Geräten oder Systemen kommunizieren muss, die auf den Standardfehler angewiesen sind.

Es ist auch wichtig zu beachten, dass `Serial.write()` die Fehlermeldung als Bytes schreibt und nicht als ASCII-Zeichen. Dies kann zu Problemen führen, wenn dein Empfänger nur ASCII-Zeichen erwartet. In diesem Fall solltest du `Serial.print()` verwenden, um die Fehlermeldung als ASCII-Zeichen zu senden.

# Siehe auch

- [Einrichten der seriellen Kommunikation mit Arduino](https://www.dr-duino.com/serielle-kommunikation-mit-arduino-einrichten)
- [Die Funktion sprintf() verstehen](https://codybonney.com/understanding-sprintf/)
- [Arduino Dokumentation zu Serial.write()](https://www.arduino.cc/reference/en/language/functions/communication/serial/write/)