---
title:    "TypeScript: Ein neues Projekt beginnen"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/typescript/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Warum

Wenn Sie sich entscheiden, ein neues Projekt zu starten, gibt es definitiv einen guten Grund dafür! Es könnte sein, dass Sie eine interessante Idee haben, die Sie verwirklichen möchten, oder dass Sie eine Herausforderung suchen, um Ihre Programmierfähigkeiten zu verbessern. Was auch immer der Grund sein mag, das Lernen von TypeScript ist eine großartige Möglichkeit, Ihre Fähigkeiten zu erweitern und gleichzeitig ein neues Projekt zu starten.

## Wie geht man vor

Bevor wir loslegen, stellen wir sicher, dass Node.js und den TypeScript Compiler installiert haben. Dann erstellen wir einen neuen Ordner für unser Projekt und initialisieren ein npm-Paket. Jetzt können wir TypeScript in unserem Projekt installieren und die Datei `tsconfig.json` erstellen. Hier definieren wir die Einstellungen für unseren TypeScript-Compiler.

```TypeScript
npm init -y
npm install typescript
npx tsc --init
```

Als Nächstes erstellen wir eine `index.ts`-Datei, in der wir unseren Code schreiben werden. In diesem Beispiel erstellen wir eine einfache Funktion, die eine Zahl als Eingabe entgegennimmt und die Quadratzahl dieser Zahl zurückgibt.

```TypeScript
function quadrat(zahl: number): number{
    return zahl * zahl;
}

console.log(quadrat(5));
```

Jetzt können wir unsere `index.ts`-Datei in JavaScript mit dem TypeScript-Compiler in die Datei `index.js` übersetzen.

```TypeScript
npx tsc
```

Wenn wir nun unsere `index.js`-Datei ausführen, wird die Quadratzahl unserer Eingabe in der Konsole ausgegeben.

```
25
```

Das war's! Wir haben erfolgreich unser erstes TypeScript-Projekt erstellt und können jetzt unseren Code mit den vielen Vorteilen von TypeScript weiterentwickeln.

## Eintauchen

Um tiefer in die Welt von TypeScript einzutauchen, können wir die offizielle Dokumentation von TypeScript lesen und verschiedene Online-Ressourcen nutzen, um unsere Fähigkeiten zu verbessern. Ein guter Einstiegspunkt ist die offizielle Webseite von TypeScript, wo wir alles über die Sprache, ihre Funktionen und ihre Anwendungen erfahren können.

Es gibt auch viele Tutorials, Blog-Beiträge und Kurse, die uns dabei helfen können, ein besseres Verständnis von TypeScript zu bekommen und unsere Fähigkeiten weiter zu verbessern. Es ist auch zu empfehlen, an Open-Source-Projekten teilzunehmen, um mehr Praxiserfahrung zu sammeln und von anderen Entwicklern zu lernen.

## Siehe Auch

- [Offizielle Website von TypeScript](https://www.typescriptlang.org/)
- [Dokumentation zu TypeScript](https://www.typescriptlang.org/docs/)
- [Von Microsoft entwickelte TypeScript-Tutorials](https://docs.microsoft.com/en-us/learn/paths/build-javascript-applications-typescript/)
- [TypeScript-Kurs auf Udemy](https://www.udemy.com/course/typescript-the-complete-developers-guide/)
- [Open-Source-Projekte auf GitHub](https://github.com/trending/typescript)