---
title:    "Arduino: Tests schreiben"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/arduino/writing-tests.md"
---

{{< edit_this_page >}}

## Warum sollten Sie beim Programmieren von Arduino Tests schreiben?

Das Schreiben von Tests ist ein wichtiger Bestandteil der Programmierung, besonders für Arduino-Projekte. Durch das Testen Ihres Codes können Sie Fehler und Probleme frühzeitig erkennen und beheben. Dadurch sparen Sie Zeit und stellen sicher, dass Ihr Projekt reibungslos funktioniert.

## So schreiben Sie Tests für Arduino:

Um Tests für Ihren Arduino-Code zu schreiben, folgen Sie diesen einfachen Schritten:

1. Definieren Sie Ihre Testfälle: Überlegen Sie sich, welche Funktionen und Abläufe in Ihrem Code getestet werden müssen.
2. Schreiben Sie Ihre Tests: Verwenden Sie die `assert()` Funktion, um zu überprüfen, ob die erwarteten Ergebnisse aus den Testfällen mit den tatsächlichen Ergebnissen übereinstimmen.
3. Führen Sie die Tests durch: Laden Sie den Code auf Ihren Arduino und überprüfen Sie die Ergebnisse der Tests.
4. Beheben Sie Fehler: Wenn ein Test fehlschlägt, überprüfen Sie Ihren Code und beheben Sie mögliche Fehler.

Hier ist ein Beispiel für einen Test, der überprüft, ob die LED an Pin 13 eingeschaltet wird:

```Arduino
void test_led() {
  LED led(13); // Erstellen Sie ein LED-Objekt an Pin 13
  
  led.on();    // Schalten Sie die LED ein
  assert(led.getState() == HIGH); // Überprüfen Sie, ob LED eingeschaltet ist
}
```

## Tiefergehende Informationen zu Tests für Arduino
Beim Schreiben von Tests sollten Sie darauf achten, dass die Tests reproduzierbar sind und unabhängig voneinander ausgeführt werden können. Verwenden Sie auch verschiedene Eingabedaten, um sicherzustellen, dass Ihre Tests alle möglichen Szenarien abdecken.

Es ist auch eine gute Idee, Ihre Tests regelmäßig auszuführen, besonders wenn Sie größere Änderungen an Ihrem Code vornehmen. Dadurch können Sie mögliche Fehler frühzeitig erkennen und beheben.

## Siehe auch
- [Offizielle Arduino-Website](https://www.arduino.cc/)
- [Arduino-Test-Framework](https://github.com/mmurdoch/arduinounit)
- [Video-Tutorial zu Tests für Arduino](https://www.youtube.com/watch?v=qa_mmf-QdtE)