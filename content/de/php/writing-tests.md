---
title:                "PHP: Tests schreiben"
simple_title:         "Tests schreiben"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/writing-tests.md"
---

{{< edit_this_page >}}

Sehr geehrte Leserinnen und Leser,

In der Welt der PHP-Programmierung ist das Verfassen von Tests ein wichtiger Bestandteil. In diesem Blog-Beitrag werde ich Ihnen erklären, warum es so wichtig ist, Tests zu schreiben und wie Sie dies effektiv tun können.

## Warum?

Bevor wir uns mit dem "Wie" beschäftigen, sollten wir uns die Frage stellen, warum wir eigentlich Tests schreiben sollten. Die Antwort ist einfach: Tests helfen uns dabei, unsere Codequalität zu verbessern und Fehler zu vermeiden. Durch das Schreiben von Tests können wir sicherstellen, dass unser Code wie erwartet funktioniert und potenzielle Probleme frühzeitig erkennen. Außerdem erleichtern Tests die Zusammenarbeit mit anderen Entwicklern, da der Code besser getestet und dokumentiert ist.

## Wie geht das?

Um Tests in PHP zu schreiben, verwenden wir in der Regel ein Framework wie PHPUnit. Zuerst definieren wir unsere Testklasse und fügen dann einzelne Testmethoden hinzu. Hier ist ein Beispiel:

```PHP
class CalculatorTest extends PHPUnit\Framework\TestCase
{
    public function testAdd()
    {
        $calculator = new Calculator();
        $result = $calculator->add(3, 5);
        $this->assertEquals(8, $result);
    }
}
```

In diesem Beispiel haben wir eine einfache Testmethode für eine `add()` Funktion des `Calculator` Objekts geschrieben. Wir können sicher sein, dass die Funktion richtig funktioniert, wenn die `assertEquals()` Methode kein Fehler zurückgibt.

Um die Testmethode auszuführen, müssen wir unser Testskript ausführen. Nachdem die Tests ausgeführt wurden, erhalten wir ein Ergebnis, das uns anzeigt, ob die Tests bestanden oder fehlgeschlagen sind und welche Tests durchgeführt wurden. Dies ist besonders nützlich, wenn unser Code wächst und wir Hunderte von Tests haben.

## Tiefere Einblicke

Beim Schreiben von Tests gibt es einige Best Practices, die wir beachten sollten. Zum Beispiel sollten wir uns auf die Funktionen konzentrieren, die am meisten genutzt werden, und diese ausgiebig testen. Außerdem sollten unsere Tests ihre eigene Logik nicht testen, sondern nur die Logik des Codes überprüfen, den sie testen sollen.

Außerdem können wir unsere Tests nutzen, um sogenannte "Edge Cases" zu testen, also ungewöhnliche Eingaben oder Situationen, die in unserem Code auftreten könnten. Dadurch stellen wir sicher, dass unser Code auch in solchen Fällen korrekt funktioniert. Es ist auch ratsam, unsere Tests regelmäßig auszuführen, um sicherzustellen, dass unser Code immer noch wie erwartet funktioniert, insbesondere wenn wir Änderungen vornehmen.

## Siehe auch

- [PHPUnit Dokumentation](https://phpunit.de/documentation.html)
- [The Art of Unit Testing in PHP](https://www.amazon.de/Art-Unit-Testing-PHP-Filler/dp/1617293733)
- [Testing PHP Applications with Codeception](https://www.amazon.de/Testing-PHP-Applications-Codeception-Cest/dp/1617296031)

Ich hoffe, dieser Blog-Beitrag hat Ihnen gezeigt, wie wichtig das Schreiben von Tests für unsere PHP-Projekte ist. Wenn Sie bisher noch keine Tests geschrieben haben, empfehle ich Ihnen, dies so schnell wie möglich zu ändern. Ihr zukünftiges Ich wird es Ihnen danken.

Auf Wiedersehen und bis zum nächsten Mal!