---
title:                "Tests schreiben"
html_title:           "TypeScript: Tests schreiben"
simple_title:         "Tests schreiben"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/writing-tests.md"
---

{{< edit_this_page >}}

# Warum und Wie Tests in TypeScript schreiben?

## Was & Warum?
Tests sind ein wichtiger Bestandteil des Programmierens, insbesondere in TypeScript. Sie dienen dazu, den Code zu überprüfen und sicherzustellen, dass er wie erwartet funktioniert. Durch das Schreiben von Tests können Programmierer potenzielle Fehler identifizieren und beheben, bevor sie in die Produktion gelangen. Das spart Zeit und vermeidet unerwünschte Probleme für die Nutzer.

## Wie geht das?
Um Tests in TypeScript zu schreiben, können verschiedene Tools und Frameworks verwendet werden, z.B. Jasmine, Jest oder Mocha. In der folgenden Code-Beispiel wird Jasmine verwendet, um eine einfache Funktion zu testen.

```TypeScript
// Funktion, die zwei Zahlen addiert
function addiere(a: number, b: number): number {
  return a + b;
}

// Test mit Jasmine
describe("addiere Funktion", () => {
  it("sollte zwei Zahlen korrekt addieren", () => {
    expect(addiere(2, 3)).toEqual(5);
  });
});
```
Beim Ausführen des Tests sollte die Ausgabe ```Pass``` lauten, was bedeutet, dass die Funktion wie erwartet funktioniert.

## Tief eintauchen
Das Schreiben von Tests ist schon lange ein wichtiger Bestandteil der Softwareentwicklung. Es gibt verschiedene Ansätze, wie z.B. Test-Driven Development (TDD), bei dem Tests vor dem eigentlichen Code geschrieben werden, oder Behavior-Driven Development (BDD), bei dem die Tests in einer natürlicheren Sprache geschrieben werden. Es ist wichtig, den für ein Projekt passenden Ansatz auszuwählen.

Alternativ können auch andere Sprachen wie JavaScript oder Python zum Schreiben von Tests verwendet werden. TypeScript bietet jedoch den Vorteil, dass die Tests direkt im selben Code wie die Anwendung geschrieben werden können und somit eine bessere Integration ermöglichen.

Für die Implementierung von Tests in TypeScript sollten die entsprechenden Frameworks oder Tools sorgfältig ausgewählt werden, abhängig von den Anforderungen des Projekts. Es ist auch wichtig, regelmäßig die Tests zu aktualisieren und zu überarbeiten, um sicherzustellen, dass sie immer noch relevant sind und potenzielle Probleme aufdecken.

## Weitere Informationen
Einige nützliche Ressourcen zum Thema Testen in TypeScript sind:

- [Jasmine Dokumentation](https://jasmine.github.io/api/3.5/global)
- [Jest Dokumentation](https://jestjs.io/docs/en/getting-started)
- [Mocha Dokumentation](https://mochajs.org/#table-of-contents)
- [Einführung in Behavior-Driven Development](https://www.agilealliance.org/glossary/bdd/)
- [TypeScript Beispielprojekt mit Tests](https://github.com/microsoft/TypeScript-Node-Starter/tree/master/src/tests)

Es ist auch hilfreich, sich mit anderen Entwicklern auszutauschen und Erfahrungen zu teilen, um das Schreiben von Tests in TypeScript zu verbessern. Mit ausreichendem Fokus auf die Qualität und die Integration von Tests können Probleme früher erkannt und die Gesamtqualität des Codes verbessert werden.