---
title:                "Verkettung von Strings"
html_title:           "Arduino: Verkettung von Strings"
simple_title:         "Verkettung von Strings"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum

Das Verketten von Zeichenfolgen ist ein nützliches Konzept beim Programmieren mit Arduino. Es ermöglicht es, verschiedene Texte zu einem zusammenzufügen und sie anschließend auszugeben oder zu speichern. Auf diese Weise können wir komplexe Aufgaben lösen und unsere Codes effizienter gestalten.

## Wie man Zeichenfolgen in Arduino verketten kann

Das Verketten von Zeichenfolgen in Arduino ist relativ einfach. Wir nutzen die `+`-Operator, um zwei oder mehrere Zeichenfolgen miteinander zu verbinden. Hier ist ein Beispiel:

```Arduino
void setup(){
  Serial.begin(9600); // Starten der seriellen Kommunikation
}

void loop(){
  String name = "Max";
  String greeting = "Hallo ";

  String message = greeting + name; // Verkettung der beiden Zeichenfolgen

  Serial.println(message); // Ausgabe: Hallo Max
  
  delay(1000); // Warten für 1 Sekunde
}
```

Die `String`-Variablen `name` und `greeting` werden miteinander verbunden und in der Variable `message` gespeichert. Anschließend wird der Inhalt von `message` über die serielle Schnittstelle ausgegeben. Das Ergebnis wird sein "Hallo Max". Beachte, dass wir keine Leerzeichen in den Strings haben, deshalb müssen wir sie manuell mit dem `+`-Operator hinzufügen.

## Tiefergehende Informationen

In Arduino können wir nicht nur Zeichenfolgen miteinander verknüpfen, sondern auch andere Datentypen wie Zahlen oder Boolesche Werte. Wir können auch eine beliebige Anzahl von Zeichenfolgen verketten, solange wir sie jeweils mit dem `+`-Operator verbinden.

Es gibt auch eine alternative Möglichkeit, Zeichenfolgen zu verketten, indem wir die `concat()`-Funktion verwenden. Diese Funktion nimmt beliebig viele Parameter entgegen und gibt eine neue Zeichenfolge zurück, die alle Parameter miteinander verbindet. Hier ist ein Beispiel:

```Arduino
void setup(){
  Serial.begin(9600); // Starten der seriellen Kommunikation
}

void loop(){
  String name = "Max";
  String intro = "Ich heiße ";
  String age = "28 Jahre alt.";

  String message = intro.concat(name, age); // Verkettung der Parameter

  Serial.println(message); // Ausgabe: Ich heiße Max28 Jahre alt.
  
  delay(1000); // Warten für 1 Sekunde
}
```

Es ist immer eine gute Idee, die `concat()`-Funktion zu verwenden, wenn wir viele Zeichenfolgen miteinander verketten müssen, da dies den Code übersichtlicher macht.

## Siehe auch

- [String-Klasse in der Arduino Reference (Englisch)](https://www.arduino.cc/reference/de/language/variables/data-types/stringobject/)
- [String-Objekte mit der `String()`-Funktion in Arduino (Englisch)](https://www.arduino.cc/en/Tutorial/StringConstructors)