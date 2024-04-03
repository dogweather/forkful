---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:07:15.503289-07:00
description: "Das Entfernen von Anf\xFChrungszeichen aus einem String in Go bedeutet,\
  \ die f\xFChrenden und abschlie\xDFenden Anf\xFChrungszeichen (`\"` oder `'`) aus\
  \ einem gegebenen\u2026"
lastmod: '2024-03-13T22:44:53.274218-06:00'
model: gpt-4-0125-preview
summary: "Das Entfernen von Anf\xFChrungszeichen aus einem String in Go bedeutet,\
  \ die f\xFChrenden und abschlie\xDFenden Anf\xFChrungszeichen (`\"` oder `'`) aus\
  \ einem gegebenen String zu entfernen."
title: "Anf\xFChrungszeichen aus einem String entfernen"
weight: 9
---

## Wie:
Go bietet verschiedene Ansätze zum Entfernen von Anführungszeichen aus einem String, aber eine der unkompliziertesten Methoden ist die Verwendung der Funktionen `Trim` und `TrimFunc`, die vom `strings`-Paket bereitgestellt werden. So geht's:

```go
package main

import (
	"fmt"
	"strings"
	"unicode"
)

func main() {
	quotedString := `"Dies ist ein 'zitierter' String"`

	// Verwendung von strings.Trim, um spezifische Anführungszeichen zu entfernen
	unquoted := strings.Trim(quotedString, `"'`)
	fmt.Println("Verwendung von strings.Trim:", unquoted)

	// Benutzerdefinierter Ansatz mit strings.TrimFunc für mehr Kontrolle
	unquotedFunc := strings.TrimFunc(quotedString, func(r rune) bool {
		return r == '"' || r == '\''
	})
	fmt.Println("Verwendung von strings.TrimFunc:", unquotedFunc)
}
```

Dieses Beispiel demonstriert zwei Ansätze, um sowohl doppelte (`"`) als auch einzelne (`'`) Anführungszeichen zu entfernen. Die Funktion `strings.Trim` ist einfacher und funktioniert gut, wenn Sie genau wissen, welche Zeichen entfernt werden sollen. Andererseits bietet `strings.TrimFunc` mehr Flexibilität und ermöglicht es Ihnen, eine benutzerdefinierte Funktion anzugeben, um zu entscheiden, welche Zeichen entfernt werden sollen. Die Beispielausgabe des obigen Codes lautet:

```
Verwendung von strings.Trim: Dies ist ein 'zitierter' String
Verwendung von strings.TrimFunc: Dies ist ein 'zitierter' String
```

Beide Methoden entfernen effektiv die führenden und abschließenden Anführungszeichen aus dem String.

## Vertiefung
Die Funktionen `Trim` und `TrimFunc` aus dem `strings`-Paket sind Teil der umfangreichen Standardbibliothek von Go, die darauf ausgelegt ist, leistungsfähige und dennoch unkomplizierte Möglichkeiten zur Stringmanipulation ohne die Notwendigkeit von Drittanbieterpaketen zu bieten. Historisch gesehen ergibt sich die Notwendigkeit, Strings effizient zu handhaben und zu manipulieren, aus dem Schwerpunkt von Go auf Netzwerkservern und Datenparsern, wo die Stringverarbeitung eine häufige Aufgabe ist.

Ein bemerkenswerter Aspekt dieser Funktionen ist ihre Implementierung auf Basis von Runen (Gos Darstellung eines Unicode-Codepunkts). Dieses Design ermöglicht es ihnen, nahtlos Strings zu handhaben, die mehrbyte Zeichen enthalten, was GOS Ansatz zur Stringmanipulation sowohl robust als auch Unicode-freundlich macht.

Während die direkte Verwendung von `Trim` und `TrimFunc` zum Entfernen von Anführungszeichen in Go bequem und idiomatisch ist, ist es erwähnenswert, dass für komplexere Stringverarbeitungsaufgaben (z.B. verschachtelte Zitate, maskierte Zitate) reguläre Ausdrücke (über das `regexp`-Paket) oder manuelles Parsen bessere Lösungen bieten könnten. Diese Alternativen gehen jedoch mit erhöhter Komplexität und Leistungsüberlegungen einher. Daher stellen die demonstrierten Methoden für einfaches Entfernen von Anführungszeichen einen guten Kompromiss zwischen Einfachheit, Leistung und Funktionalität dar.
