---
title:    "Javascript: Tests schreiben"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

# Warum
In der Webentwicklung ist es unerlässlich, qualitativ hochwertigen Code zu schreiben, der zuverlässig funktioniert. Eine Möglichkeit, dies sicherzustellen, ist durch die Implementierung von Tests. In diesem Blog-Beitrag werden wir uns genauer mit dem Schreiben von Tests in Javascript auseinandersetzen und warum es wichtig ist, dies in unseren Projekten zu tun.

# Wie geht man vor
Um mit dem Schreiben von Tests in Javascript zu beginnen, müssen wir zunächst eine Test-Bibliothek wie z.B. Jest oder Mocha installieren. Anschließend können wir unser erstes Testskript erstellen.

```Javascript
const add = (num1, num2) => {
  return num1 + num2;
};

test('Adds two numbers correctly', () => {
  expect(add(2, 3)).toBe(5);
});
```
In diesem Beispiel haben wir eine Funktion `add` definiert, die zwei Zahlen addiert. Im nächsten Abschnitt erstellen wir einen Test, um sicherzustellen, dass die Funktion wie erwartet funktioniert. Wir verwenden die `test`-Methode, um unseren Test zu benennen und die `expect`-Methode, um das erwartete Ergebnis von `add(2,3)` festzulegen. In diesem Fall sollte das Ergebnis 5 sein. Wenn unser Test erfolgreich ist, wird in der Konsole "Test passed" angezeigt. Andernfalls erhalten wir eine Fehlermeldung, die uns auf die Ursache des Fehlers hinweist.

# Tiefere Einblicke
Tests ermöglichen es uns, Codezeilen zu überprüfen und sicherzustellen, dass sie wie erwartet funktionieren. Sie dienen nicht nur als einfache Kontrolle, sondern auch als Dokumentation für unsere Funktionen. Wenn wir Tests schreiben, können wir auch Edge-Cases abdecken, die möglicherweise in der normalen Verwendung unserer Funktion auftreten können. Auf lange Sicht sparen uns gut geschriebene Tests viel Zeit und Mühe bei der Fehlersuche.

Eine weitere wichtige Sache beim Schreiben von Tests ist, dass sie uns helfen, sicherzustellen, dass bei zukünftigen Änderungen an unserem Code nichts zerbrochen wird. Wir können einfach unsere Tests erneut ausführen, um sicherzustellen, dass alles noch wie beabsichtigt funktioniert.

# Sieh auch
- https://jestjs.io/
- https://mochajs.org/
- https://www.chaijs.com/
- https://www.w3schools.com/js/js_unit_testing.asp