---
title:                "Zeichenketten verketten"
html_title:           "Go: Zeichenketten verketten"
simple_title:         "Zeichenketten verketten"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/concatenating-strings.md"
---

{{< edit_this_page >}}

# Was & Warum? 
Bei der Programmierung geht es oft darum, Informationen zu kombinieren und daraus etwas Neues zu erschaffen. Eine häufig genutzte Methode dafür ist das Zusammenfügen von Strings, also Zeichenketten, zu einem Gesamtstring. Dies ermöglicht es, verschiedene Texte oder Daten miteinander zu verbinden und so komplexe Ausgaben zu erzeugen.

# So geht's:
Go bietet verschiedene Möglichkeiten, Strings miteinander zu verbinden. Eine davon ist die Verwendung des + Operators, der zwei oder mehr Strings aneinanderreiht. Alternativ kann auch die fmt.Sprintf() Methode genutzt werden, um Platzhalter im String durch Variablen zu ersetzen. Hier ein Beispiel:

```Go
name := "John" 
age := 35

fullString := "Mein Name ist " + name + " und ich bin " + fmt.Sprintf("%d", age) + " Jahre alt."
fmt.Println(fullString)

// Ausgabe: "Mein Name ist John und ich bin 35 Jahre alt."
```

# Tiefere Einblicke:
Das Verbinden von Strings wird bereits seit den Anfängen der Programmierung genutzt, um komplexe Ausgaben zu erzeugen. Es gibt auch alternative Methoden, wie zum Beispiel das Verwenden von Vorlagen, um Strings zusammenzufügen. Bei der Umsetzung werden die einzelnen Strings in einem Array gesammelt und dann durch Iteration miteinander verbunden. 

# Weitere Infos:
Um mehr über das Verbinden von Strings in Go zu erfahren, kannst du die offizielle Dokumentation oder andere Online-Ressourcen wie Tutorials oder Foren nutzen. Folgende Links könnten hilfreich sein:

- [Offizielle Go Dokumentation](https://golang.org/doc/)
- [Tutorial: String Formatting in Go](https://www.digitalocean.com/community/tutorials/how-to-format-strings-in-go-de)
- [Stack Overflow: Concatenating Strings in Go](https://stackoverflow.com/questions/16248241/how-do-i-concatenate-two-strings-in-go)