---
date: 2024-01-26 03:42:24.697049-07:00
description: "Das Entfernen von Anf\xFChrungszeichen aus einem String bedeutet, die\
  \ umgebenden einfachen (`'`) oder doppelten (`\"`) Anf\xFChrungszeichen, die\u2026"
lastmod: '2024-03-11T00:14:27.508807-06:00'
model: gpt-4-0125-preview
summary: "Das Entfernen von Anf\xFChrungszeichen aus einem String bedeutet, die umgebenden\
  \ einfachen (`'`) oder doppelten (`\"`) Anf\xFChrungszeichen, die\u2026"
title: "Anf\xFChrungszeichen aus einem String entfernen"
---

{{< edit_this_page >}}

## Was & Warum?
Das Entfernen von Anführungszeichen aus einem String bedeutet, die umgebenden einfachen (`'`) oder doppelten (`"`) Anführungszeichen, die Zeichenfolgenliterale im Code definieren, herauszustreifen. Programmierer tun dies aus verschiedenen Gründen, wie zum Beispiel das Formatieren von Ausgaben, das Bereinigen von Benutzereingaben oder das Vorbereiten von Strings für das Parsen oder Speichern, wo die Anführungszeichen unnötig sind oder Fehler verursachen könnten.

## Wie:
Hier ist dein unkomplizierter Leitfaden, um diese lästigen Anführungszeichen von deinen Strings in TypeScript loszuwerden.

```typescript
// Option A: Einzelne oder doppelte Anführungszeichen mit Regex ersetzen
function removeQuotes(input: string): string {
  return input.replace(/^["']|["']$/g, '');
}

console.log(removeQuotes(`"Zitatierter String"`)); // Zitatierter String
console.log(removeQuotes(`'Ein weiterer'`)); // Ein weiterer

// Option B: Umgang mit Strings, die mit verschiedenen Anführungszeichen beginnen und enden
function removeMismatchedQuotes(input: string): string {
  return input.replace(/^(['"])(.*?)(?<!\1)\1$/, '$2');
}

console.log(removeMismatchedQuotes(`"Unpassende'`)); // "Unpassende'

// Option C: Entfernen verschiedener Typen von Anführungszeichen
function removeAllQuotes(input: string): string {
  return input.replace(/['"]+/g, '');
}

console.log(removeAllQuotes(`"'Mix'n'Match'"`)); // Mix'n'Match
```

## Tiefere Betrachtung
Weit zurück, bevor TypeScript überhaupt eine Sache war, hatten JavaScript-Programmierer bereits mit Anführungszeichen-Schabernack zu kämpfen, und die Geschichte ist für TypeScript ziemlich dieselbe. Mit der Zeit ändert sich auch die Art und Weise, wie wir Strings zerteilen. Heutzutage, mit der Muskelkraft von Regex, schieben wir das umständliche Slicen von Strings oder andere mühsame Methoden beiseite.

Während die oben genannten Beispiele die meisten deiner Bedürfnisse abdecken sollten, denke daran, dass das Zitieren komplex werden kann. Verschachtelte, unübereinstimmende und escape-te Anführungszeichen sind die Schlingpflanzen, die darauf warten, dich zu Fall zu bringen. Für diese benötigst du möglicherweise komplexere Muster oder sogar Parser, um jeden kniffligen Fall zu behandeln.

Alternativen? Einige Leute bevorzugen Bibliotheken wie lodash, mit Methoden wie `trim` und `trimStart` / `trimEnd`, die angepasst werden können, um Anführungszeichen abzuschneiden, wenn du die Zeichen festlegst, die du abschneiden möchtest.

Und für euch TypeScript-Enthusiasten, vergessen wir nicht die Typen. Obwohl wir uns hier hauptsächlich mit Strings beschäftigen, wenn du mit Benutzereingaben oder Parsen arbeitest, können das Hinzufügen von Typschutz oder sogar Generics helfen, sicherzustellen, dass dein Code genauso sicher ist wie deine Anführungszeichen getrimmt sind.

## Siehe auch
Besuche diese virtuellen Hotspots für weitere Informationen:

- MDN Web Docs über Regex (https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- TypeScript Offizielle Dokumentation (https://www.typescriptlang.org/docs/)
- You Don't Need Lodash/Underscore – String-Hilfsprogramme (https://github.com/you-dont-need/You-Dont-Need-Lodash-Underscore#strings)
- Stack Overflow: Durchquere die Gräben, in denen unzählige Entwickler mit Zitatkatastrophen gekämpft haben (https://stackoverflow.com/)
