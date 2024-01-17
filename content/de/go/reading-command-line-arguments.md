---
title:                "Lesen von Befehlszeilenargumenten"
html_title:           "Go: Lesen von Befehlszeilenargumenten"
simple_title:         "Lesen von Befehlszeilenargumenten"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Was & Warum? 
Beim Lesen von Befehlszeilenargumenten geht es darum, Eingaben von Benutzern zu erhalten, die beim Starten eines Programms über die Kommandozeile eingegeben werden. Programmierer tun dies, um die Funktionalität ihrer Programme anzupassen und benutzerfreundlicher zu gestalten.

## Wie geht's? 
Eine Option in Go ist die Verwendung der "Args"-Funktion aus der "os"-Paketbibliothek. Hier ein Beispiel, wie man diese Funktion verwendet:
```
package main 

import (
   "fmt"
   "os"
) 

func main() { 
   arguments := os.Args[1:] 
   fmt.Println(arguments) 
} 
```

Wenn wir dieses Programm mit dem Befehl "Go run main.go Hallo Welt" ausführen, wird "Hallo" und "Welt" als Strings zurückgegeben.

## Tieferes Eintauchen 
Die Verwendung von Befehlszeilenargumenten in Programmiersprachen ist seit den Anfängen der Befehlszeilen-Interaktion gängige Praxis. Es gibt auch alternative Möglichkeiten, dies zu tun, wie zum Beispiel das Parsen von Argumenten mit einer Flaggen- oder Argumentenparser-Bibliothek. Die Implementierung von Go's "Args"-Funktion basiert auf der "getopt"-Funktion aus dem C-Standardbibliothek.

## Siehe auch 
- [Die offizielle Go-Dokumentation zur Args-Funktion](https://golang.org/pkg/os/#Args) 
- [Ein Tutorial, das zeigt, wie man Befehlszeilenargumente mit Go nutzt](https://www.digitalocean.com/community/tutorials/how-to-read-command-line-arguments-in-go-de)