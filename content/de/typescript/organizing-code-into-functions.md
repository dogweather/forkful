---
title:                "Code in Funktionen organisieren"
date:                  2024-01-26T01:16:18.771023-07:00
model:                 gpt-4-0125-preview
simple_title:         "Code in Funktionen organisieren"

category:             "TypeScript"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Was & Warum?
Code in Funktionen zu organisieren, bedeutet, Ihren Code in wiederverwendbare, modulare Blöcke zu unterteilen. Wir tun dies, um Dinge DRY (Don't Repeat Yourself - Wiederhole dich nicht) zu halten, was den Code sauberer, leichter lesbar und einfacher zu debuggen macht.

## Wie:
Stellen Sie sich vor, Sie erstellen einen einfachen Taschenrechner. Anstatt die Logik für die Addition überall dort zu schreiben, wo Sie sie benötigen, erstellen Sie eine `add` Funktion:

```TypeScript
function add(x: number, y: number): number {
  return x + y;
}

console.log(add(5, 7)); // Beispiel-Ausgabe: 12
```

Nehmen wir nun an, wir benötigen eine Funktion zum Multiplizieren:

```TypeScript
function multiply(x: number, y: number): number {
  return x * y;
}

console.log(multiply(3, 4)); // Beispiel-Ausgabe: 12
```
Bemerken Sie, wie wir uns auf eine Aufgabe pro Funktion konzentrieren? Das ist das Herzstück der Codeorganisation.

## Tiefergehende Betrachtung
Historisch gesehen, als Programmiersprachen sich entwickelten, wurden Funktionen entscheidend für die Strukturierung von Code, angelehnt an mathematische Funktionen. Sie sind ein Grundpfeiler der prozeduralen Programmierung und leben weiter in den Paradigmen der objektorientierten und funktionalen Programmierung.

Alternativen? Man könnte einfach keine Funktionen verwenden, aber das wäre ein direkter Weg zur Spaghetti-Stadt. Oder man könnte OOP (Objektorientierte Programmierung) gehen und Funktionalitäten in Methoden packen – die im Grunde Funktionen sind, die zu Objekten gehören.

Was die Implementierung angeht, besteht TypeScript auf Typen. Eingabe- und Ausgabetypen für Funktionen zu definieren, ist nicht nur eine Frage guter Manieren; es ist ein Muss für sauberen TypeScript-Code. Plus, mit TypeScript erhalten Sie nützliche Features wie Überladungen, Generika und optionale Parameter, um Ihre Funktionen aufzuladen.

## Siehe auch
Schauen Sie sich diese Ressourcen an, um Ihr Funktionsspiel zu verbessern:

- [TypeScript-Handbuch – Funktionen](https://www.typescriptlang.org/docs/handbook/2/functions.html): Ihre Bibel für TypeScript-Funktionen.
- [Clean Code JavaScript](https://github.com/ryanmcdermott/clean-code-javascript#functions): Wenden Sie die Prinzipien des Clean Codes auf Ihre JavaScript-Funktionen an.
- [You Don’t Know JS – Scope & Closures](https://github.com/getify/You-Dont-Know-JS): Verstehen Sie, wie Funktionen mit Scope und Closures in JavaScript arbeiten.
