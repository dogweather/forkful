---
title:    "Go: Eine Zeichenfolge in Kleinbuchstaben umwandeln"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum
Das Konvertieren von Strings zu Kleinbuchstaben ist eine häufige Aufgabe in der Programmierung, besonders bei der Textverarbeitung oder der Validierung von Nutzereingaben. Es ermöglicht auch eine konsistente Behandlung von Daten, die in verschiedenen Schreibweisen vorliegen.

## Wie geht's
```Go
// Konvertieren eines Strings zu Kleinbuchstaben
s := "HALLO"
fmt.Println(strings.ToLower(s))
// Ausgabe: hallo
```
Um einen String zu Kleinbuchstaben zu konvertieren, können wir die Funktion `ToLower()` aus dem Paket `strings` verwenden. Diese Funktion nimmt einen String als Argument und gibt den konvertierten String zurück. In unserem Beispiel sehen wir, dass der String "HALLO" in "hallo" umgewandelt wurde.

Eine andere Möglichkeit ist die Verwendung der Methode `ToLower()` des Datentyps `strings.Builder`. Diese ermöglicht es, direkt auf einem String zu arbeiten, ohne eine neue Variable erstellen zu müssen.

```Go
// Verwendung von strings.Builder
s := "Hallo"
b := strings.Builder{}
b.WriteString(s)
b.WriteString(" Welt")
fmt.Println(b.String())
fmt.Println(b.String())
// Ausgabe: hallo welt
```

## Tiefergehende Informationen
Ein wichtiger Punkt beim Konvertieren von Strings zu Kleinbuchstaben ist die Beachtung der Sprachunterstützung. Verschiedene Sprachen haben unterschiedliche Regeln für die Konvertierung, die beachtet werden müssen.

Wir können auch die Unicode-Eigenschaften von Go nutzen, um sicherzustellen, dass unsere Konvertierung korrekt durchgeführt wird. Dies kann durch die Verwendung von `unicode.ToLower()` erfolgen. Diese Funktion wandelt einen einzelnen Rune (ein Zeichen im String) in einen Kleinbuchstaben um und gibt ihn zurück.

## Siehe auch
- [Dokumentation zu strings.ToLower()](https://golang.org/pkg/strings/#ToLower)
- [Dokumentation zu strings.Builder](https://golang.org/pkg/strings/#Builder)
- [Dokumentation zu unicode.ToLower()](https://golang.org/pkg/unicode/#ToLower)