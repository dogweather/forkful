---
date: 2024-01-26 03:36:21.508605-07:00
description: "Refactoring ist der Prozess der Umstrukturierung bestehenden Computer-Codes,\
  \ ohne dessen externes Verhalten zu \xE4ndern. Programmierer tun dies, um Code\u2026"
lastmod: '2024-03-13T22:44:53.639258-06:00'
model: gpt-4-0125-preview
summary: "Refactoring ist der Prozess der Umstrukturierung bestehenden Computer-Codes,\
  \ ohne dessen externes Verhalten zu \xE4ndern. Programmierer tun dies, um Code\u2026"
title: Refactoring
weight: 19
---

## Was & Warum?
Refactoring ist der Prozess der Umstrukturierung bestehenden Computer-Codes, ohne dessen externes Verhalten zu ändern. Programmierer tun dies, um Code sauberer, wartbarer zu machen und die Komplexität zu reduzieren, was ihn für jemanden, der neu einsteigt, leichter verständlich macht.

## Wie geht das:
Betrachten Sie eine TypeScript-Funktion, die schon bessere Tage gesehen hat - sie ist etwas durcheinander und könnte etwas liebevolle Pflege gebrauchen:

```typescript
function userInfo(data: any): string {
    return "User Info: " + data.name + ", " + data.age + ", " + data.email + ";";
}
```
Refaktoriert könnte dies so aussehen:

```typescript
interface User {
    name: string;
    age: number;
    email: string;
}

function formatUserInfo(user: User): string {
    return `User Info: ${user.name}, ${user.age}, ${user.email};`;
}
```

Das zweite Beispiel ist robuster und nutzt das Typsystem von TypeScript mit einem `Interface`, um potentielle Laufzeitfehler zu vermeiden und die Lesbarkeit zu verbessern.

## Tiefergehend
Refactoring ist kein modernes Konzept; es entwickelte sich mit der Programmierung und wurde mit der Veröffentlichung von Martin Fowlers Buch "Refactoring: Improving the Design of Existing Code" im Jahr 1999 formeller. Es ist entscheidend in einer agilen Entwicklungs­umgebung, um anpassungsfähige Code-Änderungen zu erleichtern. Einige Alternativen zum manuellen Refactoring umfassen automatisierte Werkzeuge wie TSLint oder den eigenen Sprachserver von TypeScript, die bestimmte Refactoring-Aufgaben für Sie vorschlagen oder sogar durchführen können. Die Implementierungsdetails umfassen in der Regel die Erkennung von "Code-Gerüchen", wie doppelten Code, lange Methoden oder große Klassen, und die Anwendung von Mustern zur Behebung – wie das Extrahieren von Methoden, die Verlagerung in besser geeignete Klassen oder die Verwendung einfacherer Konstrukte. Diese Muster sind der Schlüssel zum Verständnis des Wie und Warum des Refactorings.

## Siehe auch
- [Das Buch "Refactoring: Improving the Design of Existing Code" von Martin Fowler](https://martinfowler.com/books/refactoring.html)
- [TSLint für statische Codeanalyse](https://palantir.github.io/tslint/)
- [Verständnis von Code-Gerüchen](https://refactoring.guru/refactoring/smells)
