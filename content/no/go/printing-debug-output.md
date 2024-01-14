---
title:    "Go: Utskrift av feilrettingsutdata"
keywords: ["Go"]
---

{{< edit_this_page >}}

# Hvorfor

Å vise feilsøking utdata i programmering kan være en nyttig måte å forstå og løse problemer i koden din på. Det gir deg også bedre oversikt over hva som skjer under kjøring av programmet.

# Hvordan

For å vise debug utdata i Go, kan du bruke funksjonen `fmt.Println()` eller `fmt.Printf()`. Disse funksjonene vil skrive ut en streng eller variabel og legge til en linjeskift på slutten.

```Go
package main

import "fmt"

func main() {
    name := "Jørgen"
    fmt.Println("Hei, mitt navn er", name)
}
```

Dette vil gi følgende output:

```
Hei, mitt navn er Jørgen
```

Du kan også bruke formateringsverktøy med `fmt.Sprintf()` for å skrive ut mer komplekse debug meldinger.

```Go
package main

import "fmt"

func main() {
    age := 27
    name := "Maja"
    msg := fmt.Sprintf("Hei, mitt navn er %s og jeg er %d år gammel", name, age)
    fmt.Println(msg)
}
```

Dette vil gi følgende output:

```
Hei, mitt navn er Maja og jeg er 27 år gammel
```

# Dypdykk

Når du bruker debug utdata, kan det være lurt å inkludere informasjon om hvilken del av koden som skriver ut meldingen. Dette kan gjøres ved å bruke `runtime.Caller()`-funksjonen sammen med `fmt.Printf()`.

```Go
package main

import (
    "fmt"
    "runtime"
 )

func main() {
    _, file, line, _ := runtime.Caller(0)
    fmt.Printf("Linje %d i filen %s", line, file)
}
```

Dette vil gi følgende output:

```
Linje 9 i filen /projects/main.go
```

Husk også at for å se debug utdata i konsollen, må du kjøre programmet fra terminalen og ikke fra en IDE.

# Se også

- [Official Go documentation on debugging](https://golang.org/doc/gdb)
- [Debugging with VSCode in Go](https://code.visualstudio.com/docs/editor/debugging)