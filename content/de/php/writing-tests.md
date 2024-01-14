---
title:                "PHP: Tests schreiben"
programming_language: "PHP"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/writing-tests.md"
---

{{< edit_this_page >}}

# Warum Tests in der Programmierung wichtig sind

Jeder Programmierer kennt das unangenehme Gefühl, wenn der Code, den man stundenlang geschrieben hat, plötzlich nicht mehr funktioniert und man keine Ahnung hat, wo der Fehler liegt. Das kann zu viel Frustration und Zeitverlust führen. Hier kommen Tests ins Spiel - sie können helfen, solche Fehler frühzeitig zu erkennen und zu vermeiden. Aber warum sind Tests in der Programmierung eigentlich so wichtig?

Tests sind eine Möglichkeit, den eigenen Code auf Herz und Nieren zu prüfen. Sie decken mögliche Fehler und Bugs auf und stellen sicher, dass das Programm wie erwünscht funktioniert. Außerdem sorgen sie für eine bessere Struktur im Code, da man sich intensiv mit jeder einzelnen Funktion auseinandersetzen muss, um sinnvolle Tests dafür zu schreiben. Das kann dabei helfen, Fehler von vornherein zu vermeiden.

# Wie man Tests in PHP schreibt
Zunächst sollte man sich überlegen, welche Teile des Codes es wert sind, getestet zu werden. Oft macht es Sinn, sich auf die Kernfunktionalitäten zu konzentrieren und nicht zu viele unwichtige Details zu testen. Dann kann man mit dem Schreiben der eigentlichen Tests beginnen.

Ein Beispiel dafür könnte sein, dass man eine Funktion hat, die prüft, ob eine E-Mail-Adresse valide ist. Die Funktion könnte so aussehen:

```PHP
function validateEmail($email) {
    return filter_var($email, FILTER_VALIDATE_EMAIL);
}
```

Und so könnte ein Test dafür aussehen:

```PHP
public function testValidateEmail() {
    $this->assertTrue(validateEmail('test@test.com'), "Expecting 'true', email is valid");
    $this->assertFalse(validateEmail('test@test'), "Expecting 'false', email is invalid");
}
```

Mit diesen Tests kann man sicherstellen, dass die Funktion sowohl gültige als auch ungültige E-Mail-Adressen korrekt erkennt.

# Tiefere Einblicke in das Schreiben von Tests
Tests können in verschiedenen Arten und Umfang geschrieben werden. Man kann beispielsweise Unit Tests schreiben, die einzelne Funktionen isoliert testen, oder Integration Tests, die das Zusammenspiel mehrerer Funktionen prüfen.

Außerdem gibt es verschiedene Frameworks und Tools, die das Schreiben und Ausführen von Tests erleichtern. Eine beliebte Wahl für PHP ist zum Beispiel PHPUnit.

Es ist auch wichtig zu beachten, dass Tests kontinuierlich gepflegt und aktualisiert werden sollten. Wenn sich im Code Änderungen ergeben, müssen auch die entsprechenden Tests angepasst werden.

Das Schreiben von Tests kann anfangs etwas zeitaufwendig sein, aber es lohnt sich auf lange Sicht. Es spart Zeit und Nerven, da man mögliche Fehler frühzeitig erkennt und behebt.

# Siehe auch
- [PHPUnit Dokumentation](https://phpunit.de/documentation.html) 
- [Tutorial: Einführung in das Testen mit PHPUnit](https://www.codeception.com/docs/05-UnitTests) 
- [The PHP Testing resource List](https://thephp.cc/directory/testing.html)