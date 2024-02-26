---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:06:14.382896-07:00
description: "Das Lesen von Befehlszeilenargumenten in Go beinhaltet das Extrahieren\
  \ der Argumente, die einem Programm w\xE4hrend seiner Aufrufung aus dem Terminal\
  \ oder\u2026"
lastmod: '2024-02-25T18:49:50.505075-07:00'
model: gpt-4-0125-preview
summary: "Das Lesen von Befehlszeilenargumenten in Go beinhaltet das Extrahieren der\
  \ Argumente, die einem Programm w\xE4hrend seiner Aufrufung aus dem Terminal oder\u2026"
title: Kommandozeilenargumente lesen
---

{{< edit_this_page >}}

## Was & Warum?

Das Lesen von Befehlszeilenargumenten in Go beinhaltet das Extrahieren der Argumente, die einem Programm während seiner Aufrufung aus dem Terminal oder der Befehlsaufforderung bereitgestellt wurden. Programmierer tun dies, um die Programmausführung ohne Änderung des Codes anzupassen, was Anwendungen flexibler und benutzergesteuerter macht.

## Wie:

Go bietet direkten Zugriff auf Befehlszeilenargumente über das `os`-Paket, speziell mit `os.Args`, einem Array aus Zeichenketten. Hier ist ein einfaches Beispiel, um loszulegen:

```go
package main

import (
    "fmt"
    "os"
)

func main() {
    // os.Args bietet Zugriff auf rohe Befehlszeilenargumente
    fmt.Println("Befehlszeilenargumente:", os.Args)

    if len(os.Args) > 1 {
        // Schleifen durch die Argumente, wobei das erste argument (Programmname) übersprungen wird
        for i, arg := range os.Args[1:] {
            fmt.Printf("Argument %d: %s\n", i+1, arg)
        }
    } else {
        fmt.Println("Keine Befehlszeilenargumente bereitgestellt.")
    }
}
```

Beispielausgabe bei Ausführung mit `go run yourprogram.go arg1 arg2` könnte wie folgt aussehen:

```
Befehlszeilenargumente: [/tmp/go-build123456789/b001/exe/yourprogram arg1 arg2]
Argument 1: arg1
Argument 2: arg2
```

Dies druckt alle Argumente einschließlich des Programmnamens (oft bei Index 0) aus und iteriert dann über jedes bereitgestellte Argument und druckt es aus. Für eine kontrolliertere Argumentanalyse könnten Sie das `flag`-Paket in Betracht ziehen, um Befehlszeilenoptionen zu parsen.

## Tiefere Einblicke

Historisch gesehen ist der Zugriff auf Befehlszeilenargumente eine Praxis, die so alt wie die C-Programmierung ist, wo `argc` und `argv[]` einen ähnlichen Zweck erfüllen. In Go macht `os.Args` es unkompliziert, aber absichtlich rudimentär. Für komplexere Szenarien, wie die Handhabung von Flags oder Optionen, bietet Go das `flag`-Paket, das robuste Parsing-Fähigkeiten bereitstellt. Dies könnte als eine "bessere" Alternative angesehen werden, wenn Ihre Anwendung mehr als nur positionelle Argumente benötigt.

Im Gegensatz zu einigen Skriptsprachen, die eine integrierte Parsing von Befehlszeilenargumenten in assoziative Arrays oder Objekte anbieten, erfordert der Ansatz von Go, dass Programmierer entweder das Parsing manuell mit `os.Args` für grundlegende Bedürfnisse handhaben oder das `flag`-Paket für fortgeschrittenere Szenarien nutzen. Dieses Design spiegelt die Philosophie von Go wider, die Kernsprache einfach zu halten und gleichzeitig leistungsfähige Standardbibliotheken für gängige Aufgaben bereitzustellen. Auch wenn dies für diejenigen, die an integriertes Parsing gewöhnt sind, eine leichte Lernkurve darstellen mag, bietet es mehr Flexibilität und fördert ein tieferes Verständnis der Handhabung von Befehlszeilenargumenten.
