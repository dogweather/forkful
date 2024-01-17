---
title:                "Eine Zeichenfolge interpolieren"
html_title:           "Java: Eine Zeichenfolge interpolieren"
simple_title:         "Eine Zeichenfolge interpolieren"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Was ist das und warum?

Beim Programmieren bedeutet das Interpolieren einer Zeichenfolge, Teile der Zeichenfolge dynamisch durch Werte oder Variablen zu ersetzen. Dies ist nützlich, um eine Zeichenfolge an bestimmten Punkten anzupassen oder Daten in die Ausgabe einzufügen.

## Wie geht's?

In Java kann das Interpolieren einer Zeichenfolge auf verschiedene Arten erreicht werden. Eine Möglichkeit ist die Verwendung der "String.format ()" -Methode. Zum Beispiel:

```
String name = "Anna";
String message = String.format("Willkommen %s!", name);
System.out.println(message);
```
Die Ausgabe wäre: "Willkommen Anna!"

Alternativ können Sie auch die "+" -Operator-Methode verwenden:

```
String name = "Anna";
String message = "Willkommen " + name + "!";
System.out.println(message);
```

In beiden Fällen wird die Variable "name" in die Ausgabe eingesetzt und die Zeichenfolge angepasst.

## Tiefere Einblicke

Das Interpolieren von Zeichenfolgen wurde erstmals in der Programmiersprache Perl eingeführt und ist seitdem in vielen anderen Sprachen wie Java, Python und Ruby verfügbar. Eine Alternative zum Interpolieren von Zeichenfolgen ist die Verwendung von Konkatenation, bei der Zeichenfolgen und Variablen mit dem "+" -Operator verbunden werden.

Technisch gesehen verwendet Java die "String.format ()" -Methode intern die "StringBuffer" -Klasse, um die Zeichenfolge zu erstellen und zu manipulieren.

## Siehe auch

- [Java String.format () Dokumentation](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#format-java.lang.String-java.lang.Object...-)
- [Artikel über die Verwendung der "+ =" -Operator-Methode für die Zeichenfolgenkonkatenation in Java](https://www.baeldung.com/java-concatenation)