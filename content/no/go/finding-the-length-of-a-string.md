---
title:                "Finn lengden på en streng"
date:                  2024-01-20T17:47:46.886982-07:00
model:                 gpt-4-1106-preview
simple_title:         "Finn lengden på en streng"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å finne lengden på en streng betyr å telle antall tegn den inneholder. Programmerere gjør dette for å validere input, manipulere tekst, eller bare for å få kontroll på dataene de jobber med.

## Hvordan:
```Go
package main

import (
	"fmt"
	"unicode/utf8"
)

func main() {
	str := "Hei, verden!"
	fmt.Println("Antall bytes:", len(str))                   // Antall bytes i strengen
	fmt.Println("Antall utf8-tegn:", utf8.RuneCountInString(str)) // Antall UTF-8-tegn i strengen
}
```
Output:
```
Antall bytes: 13
Antall utf8-tegn: 12
```

## Dykk Ned:
I Go er en `string` en slags sekvens av bytes, ikke nødvendigvis bare ASCII-tegn. Før i tiden, med enkle tegnsett, var hvert tegn representert med et fast antall bytes - ofte bare ett. I Go og moderne programmering må vi tenke på Unicode, som introduserer konseptet med variabel lengde på tegn ved bruk av UTF-8 koding.

`len()`-funksjonen i Go gir oss antall bytes, ikke tegn. Fordi noen tegn kan være mer enn én byte, bruker vi `utf8.RuneCountInString()`-funksjonen for å få det faktiske antall tegn (runer) i en streng.

Et annet alternativ er å bruke en `range`-loop over strengen, som itererer over runene og ikke bytes:

```Go
str := "Hei, verden!"
count := 0
for range str {
	count++
}
fmt.Println("Antall runer:", count)
```

## Se Også:
- Go blogg på strenger: [https://blog.golang.org/strings](https://blog.golang.org/strings)
- Unicode standarden: [https://unicode.org/standard/standard.html](https://unicode.org/standard/standard.html)
- Go pakke dok for utf8: [https://pkg.go.dev/unicode/utf8](https://pkg.go.dev/unicode/utf8)