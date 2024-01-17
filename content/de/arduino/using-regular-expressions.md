---
title:                "Verwendung von regulären Ausdrücken"
html_title:           "Arduino: Verwendung von regulären Ausdrücken"
simple_title:         "Verwendung von regulären Ausdrücken"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Was & Warum?

Beim Programmieren mit dem Arduino verwenden viele Entwickler sogenannte reguläre Ausdrücke. Diese ermöglichen es, Muster in Texten oder Zeichenfolgen zu identifizieren und zu verarbeiten. Programmierer verwenden reguläre Ausdrücke, um komplexe Such- und Ersetzungsaufgaben zu automatisieren und effizienter zu gestalten.

# Wie geht's?

Die Verwendung von regulären Ausdrücken wird in der Arduino-Programmierung mithilfe des <code>Regex</code>-Objekts ermöglicht. Wir konstruieren ein neues Objekt mit dem gewünschten Muster und können dann verschiedene Methoden verwenden, um die Überprüfung und Verarbeitung von Zeichenfolgen durchzuführen.

```Arduino
#include <regex.h>

// Konstruktion des Regex-Objekts mit dem Muster "Hallo(.*)"
Regex re("Hallo(.*)");

// Überprüfung, ob die Zeichenfolge "Hallo Welt" dem Muster entspricht
if (re.match("Hallo Welt")) {
  // Ausgabe des gefangenen Textes in den Klammern mit der group() Methode
  Serial.println(re.group(1)); // Gibt " Welt" aus
}

// Ersetzung eines Teils der Zeichenfolge mit der substitute() Methode
String neueZeichenfolge = re.substitute("Hallo Welt", "Hallo Mars");
Serial.println(neueZeichenfolge); // Gibt "Hallo Mars" aus
```

# Tiefer Einblick

Die Verwendung von regulären Ausdrücken stammt ursprünglich aus dem Bereich der Textverarbeitung und hat sich in der Programmierung als sehr nützlich erwiesen. Alternativen zu regulären Ausdrücken sind z.B. die Verwendung von String-Methoden oder regelbasierte Such- und Ersetzungsfunktionen. Die Implementierung von regulären Ausdrücken in der Arduino-Programmierung basiert auf der C++-Bibliothek <code>regex.h</code>.

# Siehe auch

- Offizielle Dokumentation zur <code>Regex</code>-Klasse: https://www.arduino.cc/reference/en/language/functions/regular-expressions/
- Einführung in reguläre Ausdrücke: https://regexone.com/