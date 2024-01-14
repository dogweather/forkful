---
title:    "Arduino: Suchen und Ersetzen von Text"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Warum

Das Suchen und Ersetzen von Text kann eine nützliche Fähigkeit sein, wenn man mit Arduino-Programmierung arbeitet. Es ermöglicht einem, schnell und effizient Textübertragungen oder Bearbeitungen in seinem Code durchzuführen. In diesem Blog-Beitrag werden wir uns genauer damit befassen, wie man dieses Werkzeug in der Arduino-Umgebung einsetzen kann.

## Wie geht es

Um Text in einem Arduino-Code zu suchen und zu ersetzen, gibt es zwei nützliche Funktionen: `find()` und `replace()`. Die `find()`-Funktion ermöglicht es einem, eine Zeichenkette in einem Text zu suchen, während die `replace()`-Funktion es einem ermöglicht, diese Zeichenkette durch eine andere zu ersetzen. Schauen wir uns ein Beispiel an:

```Arduino
String text = "Hallo, ich heiße Arduino.";

//Suche nach "Arduino" und ersetze es durch "Max"
text.replace("Arduino", "Max");

Serial.println(text); //Ausgabe: "Hallo, ich heiße Max."
```

Wie man sehen kann, wird die Zeichenkette "Arduino" in `text` durch "Max" ersetzt. Zusätzlich zur Verwendung von konstanten Zeichenketten kann man auch Variablen in `find()` und `replace()` verwenden. Schauen wir uns ein weiteres Beispiel an:

```Arduino
String name = "Max";
String text = "Hallo, ich heiße Arduino.";

//Suche nach name und ersetze es durch "Max"
text.replace(name, "Max");

Serial.println(text); //Ausgabe: "Hallo, ich heiße Max."
```

Dies kann besonders nützlich sein, wenn man Namen oder andere Variablen in seinem Code ändern möchte, ohne jedes Mal die Zeichenkette manuell zu ersetzen.

## Tiefer eintauchen

Die `find()` und `replace()` Funktionen bieten noch weitere Optionen, um die Suche und das Ersetzen von Text zu verfeinern. So kann man zum Beispiel angeben, bei welchem Zeichen man mit der Suche beginnen möchte oder wie oft man die zu ersetzende Zeichenkette finden und ersetzen möchte. Hier sind einige nützliche Links, um tiefer in diese Funktionen einzutauchen:

- [String Funktionen Referenz](https://www.arduino.cc/reference/de/language/variables/data-types/string/functions/?from=reference/en#string-functions)
- [Tutorial zur Verwendung von `find()` und `replace()`](https://www.arduino.cc/en/Tutorial/StringReplace)
- [String-Objekt Referenz](https://www.arduino.cc/reference/de/language/variables/data-types/string/)

## Siehe auch

Für weitere nützliche Ressourcen zur Arduino-Programmierung empfehle ich die folgenden Links:

- [Arduino Tutorials auf Deutsch](https://www.arduino.cc/en/Tutorial/BuiltInExamples?from=Tutorial.TutorialList)
- [Forum für deutsche Arduino-Programmierer](https://forum.arduino.cc/index.php?board=24.0)
- [Deutsche Arduino Community auf Facebook](https://www.facebook.com/groups/471749809527679/)

Happy coding!