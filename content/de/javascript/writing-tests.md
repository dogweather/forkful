---
title:    "Javascript: Tests schreiben"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/javascript/writing-tests.md"
---

{{< edit_this_page >}}

## Warum

Es gibt viele Gründe, warum es sinnvoll ist, Tests beim Programmieren zu schreiben. Einer der wichtigsten Gründe ist, dass Tests sicherstellen, dass der Code ordnungsgemäß funktioniert und keine unerwarteten Fehler auftreten. Dadurch wird die Code-Qualität verbessert und die Entwicklungszeit verkürzt.

## Wie man Tests schreibt
Um Tests zu schreiben, müssen wir zuerst sicherstellen, dass wir eine Testumgebung haben, die unsere Tests ausführen kann. In der Regel wird dafür ein Test-Framework wie Mocha oder Jasmine verwendet.

Als nächsten Schritt müssen wir unsere Tests in separate Dateien schreiben und sicherstellen, dass unser Code in einzelne Funktionen unterteilt wird, die leichter getestet werden können. Hier ist ein Beispiel für einen Test in Mocha:

```Javascript
describe('sum function', function() {
    it('should return the sum of two numbers', function() {
        expect(sum(1,2)).toEqual(3);
    })
})
```

In diesem Beispiel testen wir die Summenfunktion, indem wir überprüfen, ob sie die korrekte Summe von 1 und 2 zurückgibt.

Nachdem wir unsere Tests geschrieben haben, können wir sie ausführen, indem wir die Befehle "npm test" oder "mocha" in der Kommandozeile eingeben.

## Tiefer Einblick
Beim Schreiben von Tests ist es wichtig, alle möglichen Szenarien abzudecken, einschließlich der Kantenfälle. Durch das Schreiben von umfassenden Tests können wir sicherstellen, dass unser Code robust und fehlerfrei ist.

Eine wichtige Technik beim Testen ist das "Test-driven Development" (TDD), bei dem Tests vor der eigentlichen Entwicklung geschrieben werden. Dadurch wird sichergestellt, dass der Code von vornherein gut getestet ist und die Entwickler sich auf das Schreiben funktionierenden Codes konzentrieren können.

Eine weitere wichtige Facette des Testens ist das "Mocking". Durch das Mocking von Objekten oder Funktionen können wir Tests auf unabhängige Teile unseres Codes beschränken und so die Effizienz und Genauigkeit unserer Tests erhöhen.

## Siehe auch
- [Mocha](https://mochajs.org/)
- [Jasmine](https://jasmine.github.io/)
- [Test-driven Development](https://de.wikipedia.org/wiki/Testgetriebene_Entwicklung)
- [Mocking](https://en.wikipedia.org/wiki/Mock_object)