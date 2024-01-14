---
title:    "Go: Suchen und Ersetzen von Text"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Warum

In jedem Entwicklungsprojekt gibt es immer wieder die Notwendigkeit, Text zu suchen und zu ersetzen. Dies kann aus verschiedenen Gründen erforderlich sein, beispielsweise um Fehler zu korrigieren oder um eine einheitliche Schreibweise zu gewährleisten. In diesem Blog-Beitrag werden wir uns damit beschäftigen, wie man Textsuche und -ersetzungen in Go Programmierung durchführt.

## Wie geht's

Um Text in Go zu suchen und zu ersetzen, gibt es eine Reihe von nützlichen Funktionen. Die `strings` Paket bietet eine Vielzahl von Methoden für die Arbeit mit Zeichenketten, darunter auch Funktionen für die Suche und den Austausch von Text. Schauen wir uns einige Beispiele an:

```Go
// Eine Zeichenkette erstellen
str := "Hallo Welt!"

// Textsuche
if strings.Contains(str, "Welt") {
    fmt.Println("Die Zeichenfolge enthält das Wort 'Welt'.")
}

// Text ersetzen
newStr := strings.ReplaceAll(str, "Hallo", "Guten Tag")
fmt.Println(newStr) // Guten Tag Welt!

// Nur das erste Auftreten des Suchtexts ersetzen
newStr := strings.Replace(str, "l", "LL", 1)
fmt.Println(newStr) // HalLO Welt!
```

In den obigen Beispielen haben wir die Methoden `Contains()`, `ReplaceAll()` und `Replace()` verwendet, um zu überprüfen, ob ein Text vorhanden ist und ihn gegebenenfalls zu ersetzen. Es gibt noch weitere nützliche Funktionen, wie z.B. `Count()`, um die Anzahl der Vorkommen eines bestimmten Textes zu zählen, oder `ToLower()`/`ToUpper()` um die Schreibweise zu ändern.

## Tiefentauchen

Ein wichtiger Aspekt bei der Suche und dem Ersatz von Text ist die Verwendung von regulären Ausdrücken (RegEx). Mit RegEx können Sie komplexe Suchmuster definieren und ersetzen. Das `regexp` Paket in Go bietet Funktionen zum Kompilieren und Ausführen von RegEx-Ausdrücken.

Hier ist ein Beispiel für die Verwendung von RegEx in Go:

```Go
// RegEx kompilieren und ausführen
re := regexp.MustCompile(`\d{2}-\d{2}-\d{4}`)
matched := re.MatchString("08-16-2021")

if matched {
    fmt.Println("Datum gefunden.")
}

// Text ersetzen mit RegEx
newStr := re.ReplaceAllString("Geburtsdatum: 08-16-2021", "Geboren am: ")
fmt.Println(newStr) // Geburtsdatum: Geboren am: 
```

In diesem Beispiel suchen wir nach einem Datumsmuster (TT-MM-JJJJ) in einem Text und ersetzen es mit der gewünschten Ausgabe. RegEx kann jedoch noch viel komplexer werden und ist ein mächtiges Werkzeug für die Textbearbeitung.

## Siehe auch

- [Official Go Documentation](https://golang.org/pkg/strings/)
- [A Tour of Go: String functions](https://tour.golang.org/basics/21)
- [Mastering Go: Search and Replace](https://www.packtpub.com/product/mastering-go-second-edition/9781788626545)