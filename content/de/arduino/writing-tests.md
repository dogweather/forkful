---
title:    "Arduino: Tests schreiben"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Warum

Arduino ist ein beliebtes Programmierboard für Bastler, Tüftler und Elektronik-Enthusiasten. Viele Menschen lieben es, damit ihre eigenen Projekte zu verwirklichen. Doch egal, ob man ein einfaches LED-Blink-Script schreibt oder ein komplexes Roboterprojekt umsetzt, es ist wichtig, Tests in den Programmierprozess einzubeziehen. Dieser Blog-Beitrag erklärt, warum das Schreiben von Tests für Arduino-Projekte von Bedeutung ist.

## Wie geht das?

Das Schreiben von Tests für Arduino-Projekte ist ähnlich wie das Schreiben von Tests für andere Programmiersprachen. Wir müssen uns auf das Auftreten von bestimmten Ereignissen oder Ausgaben konzentrieren und überprüfen, ob sie den Erwartungen entsprechen. Wir werden dies anhand einer einfachen LED-Blink-Anwendung demonstrieren.

```Arduino
// Hier importieren wir die benötigten Bibliotheken.
#include <Arduino.h>
#include <unity.h>

// Hier definieren wir, welche Pins wir für unsere LED verwenden möchten.
const int LED_PIN = 3;

// In der Setup-Funktion initialisieren wir unseren LED-Pin als Output.
void setup()
{
    pinMode(LED_PIN, OUTPUT);
}

// Die Blink-Funktion lässt die LED blinken.
void blink()
{
    digitalWrite(LED_PIN, HIGH);
    delay(1000);
    digitalWrite(LED_PIN, LOW);
    delay(1000);
}

// Testen der Blink-Funktion.
void test_blink()
{
    // Wir erwarten, dass unsere LED drei Mal blinkt.
    for (int i = 0; i < 3; i++)
    {
        blink();
        // Hier überprüfen wir, ob die LED den erwarteten Zustand erreicht hat.
        TEST_ASSERT_EQUAL(digitalRead(LED_PIN), HIGH);
        delay(1000);
        TEST_ASSERT_EQUAL(digitalRead(LED_PIN), LOW);
        delay(1000);
    }
}

// Die Hauptfunktion lädt die Tests und führt sie aus.
void loop()
{
    UNITY_BEGIN();
    RUN_TEST(test_blink);
    UNITY_END();
}
```

Das ist alles, was wir tun müssen, um Tests für unsere Blink-Funktion zu schreiben. Wenn wir dieses Skript auf unserem Arduino ausführen, werden die Tests ausgeführt und wir erhalten ein Ergebnis, ob die Funktion wie erwartet funktioniert. In diesem Fall sollten wir ein erfolgreiches Testergebnis erhalten.

## Tiefegründung

Tests helfen uns, die Funktionalität unserer Anwendungen sicherzustellen und Fehler frühzeitig zu erkennen. Besonders bei Arduino-Projekten, bei denen es um physische Komponenten geht, können Tests uns vor unvorhergesehenen Problemen schützen. Durch das Schreiben von Tests können wir auch sicherstellen, dass unsere Anwendungen zuverlässig und stabil funktionieren.

Abgesehen davon, dass sie uns helfen, Fehler zu erkennen, machen Tests auch den Programmierprozess effizienter. Durch das Schreiben von Tests können wir unser Code-Design verbessern und einfacher und schneller reagieren, wenn wir neue Funktionen hinzufügen müssen.

## Siehe auch

- [Arduino offizielle Website](https://www.arduino.cc/)
- [Unity für Arduino GitHub Repository](https://github.com/arduino-libraries/ArduinoUnit)
- [Test-Driven Development für Arduino-Projekte](https://www.smashingmagazine.com/2018/07/test-driven-development-arduino/)