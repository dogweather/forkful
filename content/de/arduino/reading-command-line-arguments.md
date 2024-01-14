---
title:    "Arduino: Lesen von Befehlszeilenargumenten"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

# Warum

In diesem Blog-Post erfahren Sie, warum es wichtig ist, Befehlszeilenargumente in der Arduino-Programmierung zu verstehen und wie Sie diese in Ihrem Code nutzen können.

# Wie man Befehlszeilenargumente in Arduino liest

Die Befehlszeilenargumente sind Parameter, die beim Starten eines Programms übergeben werden und es Ihnen ermöglichen, Ihr Programm je nach Bedarf anzupassen. In Arduino können Befehlszeilenargumente über die Funktion ```Serial.readString()``` gelesen werden.

Hier ist ein Beispielcode, der einen Befehlszeilenparameter mit dem Namen "message" erwartet und diesen dann auf dem seriellen Monitor ausgibt:

```Arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  if (Serial.available()) {
    String message = Serial.readStringUntil('\n'); // liest den Befehlszeilenparameter bis zur Zeilenumbruch-Zeichenfolge ('\n')
    Serial.println("Empfangene Nachricht: " + message);
  }
}
```

Wenn wir nun das Programm starten und im seriellen Monitor "message:Hallo!" eingeben, wird "Empfangene Nachricht: Hallo!" ausgegeben.

# Tiefgehende Informationen zum Lesen von Befehlszeilenargumenten

Um Befehlszeilenargumente effektiv nutzen zu können, ist es wichtig zu verstehen, wie sie strukturiert sind. Befehlszeilenargumente bestehen aus einem Namen, gefolgt von einem Doppelpunkt und dem Wert des Parameters. Bei mehreren Befehlszeilenargumenten werden diese durch ein Leerzeichen getrennt.

In unserem Beispielcode wäre "message" der Name des Parameters und "Hallo!" der Wert.

Es ist auch möglich, Befehlszeilenargumente in einem Array zu speichern, um sie später in Ihrem Code zu verwenden. Hier ist ein Beispiel, wie dies umgesetzt werden kann:

```Arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  if (Serial.available()) {
    String message = Serial.readStringUntil('\n'); // liest den Befehlszeilenparameter bis zur Zeilenumbruch-Zeichenfolge ('\n')

    // Teilt den String an dem Doppelpunkt
    int colonIndex = message.indexOf(':');
    String parameterName = message.substring(0, colonIndex); // extrahiert den Namen des Parameters
    String parameterValue = message.substring(colonIndex + 1); // extrahiert den Wert des Parameters

    Serial.println("Parametername: " + parameterName);
    Serial.println("Parametervalue: " + parameterValue);
  }
}
```

Wenn wir nun im seriellen Monitor "message:Hallo!" eingeben, wird "Parametername: message" und "Parametervalue: Hallo!" ausgegeben.

# Siehe auch

- [Serial.readString()](https://www.arduino.cc/reference/de/language/functions/communication/serial/readstring/)
- [String-Methoden in Arduino](https://www.arduino.cc/reference/de/language/variables/data-types/stringobject/)
- [Befehlszeilenargumente erklärt in einfachen Worten](https://www.howtogeek.com/435288/what-are-command-line-arguments-and-how-do-you-use-them/)