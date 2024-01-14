---
title:    "Go: Lesen von Befehlszeilenargumenten"
keywords: ["Go"]
---

{{< edit_this_page >}}

# Warum

Das Lesen von Befehlszeilenargumenten ist eine wichtige Fähigkeit für jeden Go-Programmierer. Es ermöglicht die Interaktion mit dem Programm über die Kommandozeile und erleichtert die Verwendung und Anpassung des Programms.

# Wie man es macht

Um Befehlszeilenargumente in Go zu lesen, verwenden wir die os.Args-Funktion. Diese Funktion gibt ein Array mit den Argumenten zurück, die beim Aufruf des Programms übergeben wurden.

```Go
func main() {
    args := os.Args
    fmt.Println("Argumente:", args)
}
```

Die Ausgabe dieses Programms wäre folgende:

```Shell
$ ./programm arg1 arg2
Argumente: ./programm arg1 arg2
```

Wie wir sehen können, enthält das Array auch den Namen des Programms als ersten Eintrag. Wir können nun über das Array iterieren und die Argumente verwenden, um unser Programm anzupassen oder zu steuern.

# Tiefere Einblicke

Um noch tiefer in das Thema einzusteigen, können wir uns die os.Args-Funktion genauer ansehen. Diese Funktion verwendet ein globales Array namens "osArgs" und fügt alle übergebenen Argumente als Elemente zu diesem Array hinzu.

Wir können auch die Indexfunktion verwenden, um auf spezifische Argumente zuzugreifen:

```Go
func main() {
    args := os.Args
    fmt.Println("Erstes Argument:", args[1])
    fmt.Println("Zweites Argument:", args[2])
}
```

Die Ausgabe würde folgendermaßen aussehen:

```Shell
$ ./programm arg1 arg2
Erstes Argument: arg1
Zweites Argument: arg2
```

Außerdem können wir die Länge des Arrays mit der len-Funktion überprüfen, um zu sehen, wie viele Argumente übergeben wurden:

```Go
func main() {
    args := os.Args
    fmt.Println("Anzahl der Argumente:", len(args))
}
```

Die Ausgabe würde folgendermaßen lauten:

```Shell
$ ./programm arg1 arg2
Anzahl der Argumente: 3
```

# Siehe auch

- [Offizielle Dokumentation zu os.Args](https://golang.org/pkg/os/#Args)
- [Videoanleitung zu os.Args](https://www.youtube.com/watch?v=F5FeWrkggdc)
- [Weitere Informationen zu Kommandozeilenargumenten in Go](https://gobyexample.com/command-line-arguments)