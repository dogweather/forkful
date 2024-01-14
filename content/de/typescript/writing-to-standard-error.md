---
title:                "TypeScript: Schreiben auf Standardfehler"
programming_language: "TypeScript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Warum
Es gibt viele Gründe, warum ein Entwickler möglicherweise auf die Standardfehlerausgabe (standard error) zugreifen muss. Zum Beispiel kann es hilfreich sein, um Fehlermeldungen oder Debugging-Informationen auszugeben, die nicht auf der Standardausgabe (standard output) erscheinen sollen. Es kann auch nützlich sein, um spezielle Benachrichtigungen oder Warnungen auszugeben, die in einem separaten Stream dargestellt werden sollen.

## Wie geht man vor
Um auf die Standardfehlerausgabe zuzugreifen, kann man in TypeScript die Konsole (console) Objekt verwenden. Hier ist ein Beispiel, wie man eine Fehlermeldung auf die Standardfehlerausgabe ausgibt:

```TypeScript
console.error("Es ist ein Fehler aufgetreten.");
```

Dies wird eine rote Fehlermeldung in der Konsole anzeigen. Um eine Warnung auszugeben, können Sie "console.warn" anstelle von "console.error" verwenden. Hier ist ein Beispiel:

```TypeScript
console.warn("Achtung: Eine wichtige Funktion wird in Kürze entfernt.");
```

Dies wird eine gelbe Warnung in der Konsole anzeigen. Man kann auch beliebige Objekte oder Variablen in den Befehlen angeben, um erweiterte Informationen auszugeben. Zum Beispiel:

```TypeScript
const name = "Max";
console.error(`Der Name ${name} ist ungültig.`);
```

Dies wird eine Fehlermeldung mit dem spezifischen Namen ausgeben.

## Tiefere Einblicke
Die Standardfehlerausgabe kann auch in Kombination mit Try-Catch-Blöcken verwendet werden, um Fehler in einer Anwendung zu erfassen und auszugeben. Zum Beispiel:

```TypeScript
try {
    // Hier steht der Code, der möglicherweise einen Fehler verursachen kann
} catch (error) {
    console.error("Es ist ein Fehler aufgetreten: ", error);
}
```

Dies kann hilfreich sein, um Probleme in einer Anwendung zu erkennen und beheben zu können.

## Siehe auch
- [Offizielle TypeScript Dokumentation über die Konsole](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-3.html)
- [Artikel über den Unterschied zwischen Standardausgabe und Standardfehlerausgabe](https://www.lifewire.com/what-are-stdin-stdout-and-stderr-2626133)
- [Beispielprojekt mit der Verwendung der Standardfehlerausgabe in TypeScript](https://github.com/example-project)

Vielen Dank, dass Sie diesen Artikel über das Schreiben zur Standardfehlerausgabe in TypeScript gelesen haben. Wir hoffen, dass es Ihnen geholfen hat, besser zu verstehen, wie Sie diese Funktion in Ihren Projekten nutzen können. Bis zum nächsten Mal!