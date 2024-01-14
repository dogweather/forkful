---
title:                "C: Fehlersuchausgabe drucken"
simple_title:         "Fehlersuchausgabe drucken"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum

Debug-Ausgaben sind ein wesentlicher Bestandteil der Programmierung. Sie helfen dabei, Fehler in unserem Code zu finden und zu beheben. Durch das Drucken von Debug-Ausgaben können wir den Ablauf unseres Programms verfolgen und überprüfen, ob die Variablen die erwarteten Werte haben. Dies trägt erheblich zu einer effizienten Fehlerbehebung bei und spart uns Zeit während der Entwicklung.

## Wie geht das?

Um Debug-Ausgaben in C zu drucken, verwenden wir die Funktion `printf()`. Diese Funktion erlaubt es uns, Variablen und Text auf dem Bildschirm auszugeben. Schauen wir uns ein Beispiel an:

```C
int num = 10;
printf("Die Zahl ist: %d", num);
```

In diesem Beispiel wird die Variable `num` mithilfe des Format-Spezifiers `%d` ausgegeben. Dies gibt den Wert von `num` als Ganzzahl aus. Wir können auch mehrere Variablen und Text kombinieren:

```C
int num1 = 5;
int num2 = 7;
printf("Summe der Zahlen %d und %d ist: %d", num1, num2, (num1 + num2));
```

Das Ergebnis dieser Ausgabe wäre: "Summe der Zahlen 5 und 7 ist: 12".

## Tiefere Einblicke

Um Debug-Ausgaben noch effektiver zu gestalten, können wir auch Formatierungsanweisungen verwenden. Diese Anweisungen ermöglichen es uns, Variablenwerte mit bestimmten Formaten auszugeben, beispielsweise als Hexadezimalzahlen oder mit einer bestimmten Anzahl von Nachkommastellen. Schauen wir uns ein Beispiel an:

```C
float pi = 3.14159265359;
printf("Der Wert von pi ist: %.2f", pi);
```

Das Ergebnis dieser Ausgabe wäre: "Der Wert von pi ist: 3.14". In diesem Beispiel haben wir mit der Formatierung `%.2f` angegeben, dass die Ausgabe des Float-Wertes `pi` auf zwei Nachkommastellen begrenzt werden soll.

Es ist wichtig zu beachten, dass Debug-Ausgaben nicht in der endgültigen Version unseres Programms enthalten sein sollten. Sie sind nur zur Unterstützung während der Entwicklungsphase gedacht und sollten später entfernt werden.

## Siehe auch

- [Verwendung von printf in C](https://www.tutorialspoint.com/c_standard_library/c_function_printf.htm)
- [Debugging-Tipps für C-Programmierer](https://www.ibm.com/support/knowledgecenter/en/SSLTBW_2.3.0/com.ibm.zos.v2r3.bpxbd00/dbgdbg.htm)