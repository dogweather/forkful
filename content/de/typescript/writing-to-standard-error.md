---
title:    "TypeScript: Schriftliches Schreiben auf den Standardfehler"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Warum

Manchmal kann es beim Programmieren passieren, dass ein Fehler auftritt. Oder vielleicht möchtest du einfach nur etwas ausgeben, um zu überprüfen, ob dein Code an der richtigen Stelle ausgeführt wird. In solchen Situationen kann es nützlich sein, die Funktion "writing to standard error" zu verwenden. Diese Funktion gibt eine Ausgabe auf der Standardfehlerschnittstelle, auch bekannt als "stderr", aus.

## Wie geht man vor

Um in TypeScript auf standard error zu schreiben, kannst du die Methode `console.error()` verwenden. Hier ist ein Beispielcode:

```TypeScript
const name = "Peter";
console.error(`Hallo ${name}, dieses Beispiel hat eine Fehlermeldung ausgegeben.`);
```

Die Ausgabe sieht dann wie folgt aus:

```bash
Hallo Peter, dieses Beispiel hat eine Fehlermeldung ausgegeben.
```

Du kannst auch mehrere Werte ausgeben, indem du sie mit Kommas trennst:

```TypeScript
console.error("Fehler:", 404, "Seite nicht gefunden!");
```

Die Ausgabe hierfür wäre:

```bash
Fehler: 404 Seite nicht gefunden!
``` 

## Deep Dive

Wenn ein Fehler in deinem Code auftritt, wird dieser normalerweise auf der Standardausgabeschnittstelle, oder "stdout", ausgegeben. Dies ist jedoch nicht immer das gewünschte Verhalten. Auch wenn dein Programm nicht abstürzt, kann es hilfreich sein, gewisse Informationen auf stderr auszugeben, um bessere Debugging-Möglichkeiten zu haben. Eine andere Verwendung von stderr ist es, Benutzereingaben in einer Befehlszeileanwendung zu validieren.

Es ist auch wichtig zu beachten, dass stderr und stdout getrennte Ausgabekanäle sind und daher die Ausgaben in der Reihenfolge angezeigt werden, in der sie ausgegeben wurden. Das bedeutet, dass die Ausgabe auf stderr nicht unbedingt direkt unter der Ausgabe auf stdout erscheint, sondern abhängig von der Reihenfolge, in der die jeweilige Ausgabe erfolgt ist.

## Siehe auch

- [Die Konsole in Node.js nutzen](https://www.digitalocean.com/community/tutorials/nodejs-command-line-utility-console)
- [Die Unterschiede zwischen stderr und stdout](https://www.quora.com/What-is-the-difference-between-standard-error-and-standard-output-in-Linux)
- [Die console.error() Methode in TypeScript](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-3.html#improved-quick-fixes)