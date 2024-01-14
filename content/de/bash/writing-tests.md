---
title:                "Bash: Tests schreiben"
programming_language: "Bash"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/writing-tests.md"
---

{{< edit_this_page >}}

## Warum

bash programming hat sich in den letzten Jahren zu einer beliebten Methode der Entwicklung von Automatisierungs- und Skripting-Tools entwickelt. Einer der wichtigsten Aspekte dieser Programmierung ist das Testen des Codes, um sicherzustellen, dass er zuverlässig und fehlerfrei funktioniert. Das Schreiben von Tests ermöglicht es Entwicklern, Probleme frühzeitig zu erkennen und zu beheben, was Zeit und Frustration bei der Entwicklung sparen kann. In diesem Blog-Beitrag werden wir uns genauer ansehen, wie man effektive Tests in Bash schreibt.

## Wie man Tests in Bash schreibt

Das Schreiben von Tests in Bash ist relativ einfach und erfordert keine speziellen Vorkenntnisse. Hier ist ein Beispiel für eine Funktion, die einen String umkehrt:

```Bash
# Funktion zum Umkehren eines Strings
function reverse_string {
    # Überprüfen, ob ein String übergeben wurde
    if [ $# -eq 0 ]
    then
        echo "Bitte geben Sie einen String als Argument ein."
        exit 1
    fi
    # Loop über alle Buchstaben im String und füge sie in umgekehrter Reihenfolge zu einem neuen String zusammen
    local reversed_string=""
    for (( i=${#1} - 1; i >= 0; i-- ))
    do
        reversed_string+="${1:i:1}"
    done
    echo "$reversed_string"
}
```
Dies ist eine grundlegende Funktion, die jedoch viele verschiedene Fehler enthalten könnte. Wir können nun Tests schreiben, um sicherzustellen, dass die Funktion zuverlässig funktioniert. Hier ist ein Beispiel dafür:

```Bash
function test_reverse_string {
    # Überprüfen, ob die Funktion einen leeren String richtig umkehrt
    if [ $(reverse_string "") != "" ]
    then
        echo "Fail: Funktion gibt einen falschen Wert zurück."
    else
        echo "Pass: Funktion gibt den korrekten Wert zurück."
    fi
    # Überprüfen, ob die Funktion einen einzelnen Buchstaben richtig umkehrt
    if [ $(reverse_string "a") != "a" ]
    then
        echo "Fail: Funktion gibt einen falschen Wert zurück."
    else
        echo "Pass: Funktion gibt den korrekten Wert zurück."
    fi
    # Überprüfen, ob die Funktion einen längeren String richtig umkehrt
    if [ $(reverse_string "Hallo Welt") != "tleW ollaH" ]
    then
        echo "Fail: Funktion gibt einen falschen Wert zurück."
    else
        echo "Pass: Funktion gibt den korrekten Wert zurück."
    fi
}
# Aufrufen der Test-Funktion
test_reverse_string
```

Die Ausgabe sollte folgendermaßen aussehen:

```Bash
Pass: Funktion gibt den korrekten Wert zurück.
Pass: Funktion gibt den korrekten Wert zurück.
Pass: Funktion gibt den korrekten Wert zurück.
```

Mit diesen Tests haben wir nun überprüft, ob unsere Funktion leere Strings, einzelne Buchstaben und längere Strings korrekt umkehrt. Wir können auch weitere Tests hinzufügen, um sicherzustellen, dass die Funktion mit Sonderzeichen oder Zahlen umgehen kann.

## Tiefergehende Informationen über das Schreiben von Tests

Das Schreiben von Tests sollte nicht als lästige Aufgabe betrachtet werden, sondern als wichtiger Teil des Entwicklungsprozesses. Es ermöglicht es uns, sicherzustellen, dass unser Code zuverlässig und fehlerfrei funktioniert, sowie Änderungen oder Hinzufügungen im Code zu überprüfen. Es ist auch eine gute Möglichkeit, um sicherzustellen, dass unser Code gut dokumentiert und verständlich ist.

Es gibt viele verschiedene Test-Frameworks für Bash, die Entwickler nutzen können. Einige beliebte Beispiele sind [Bats](https://github.com/sstephenson/bats), [shUnit2](https://github.com/kward/shunit2) und [bats-mock](https://github.com/jasonkarns/bats-mock). Diese Frameworks bieten zusätzliche Funktionen wie Assertions, Mocking und Coverage-Tests, die das Schreiben von Tests noch einfacher und effektiver machen.

Es ist auch wichtig, die Tests regelmäßig auszuführen und bei Änderungen im Code zu aktualisieren. Auf diese Weise können potenzielle Fehler früh