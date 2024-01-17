---
title:                "Puuttuvien parametrien lukeminen"
html_title:           "Go: Puuttuvien parametrien lukeminen"
simple_title:         "Puuttuvien parametrien lukeminen"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Lue komentoriviparametrien luominen tarkoittaa rivien sisältöjen lukemista, kun ohjelma käynnistetään terminaalissa. Ohjelmoijat tekevät sitä, jotta he voivat välittää tietoja ohjelmalle jo ennen sen suorittamista.

## Kuinka:

```Go
package main 

import "fmt" 
import "os" 

func main() { 
    argsWithProg := os.Args 
    argsWithoutProg := os.Args[1:] 
 
    arg := os.Args[3] 
    fmt.Println(argsWithProg) 
    fmt.Println(argsWithoutProg) 
    fmt.Println(arg) 
}
```
Esimerkkituloste:

```
$ go run command_line_arguments.go a b c d 
[/path/to/command_line_arguments a b c d] 
[a b c d] 
c
```

## Syvällinen syvennys:

Komentoriviparametrien lukeminen on yleinen käytäntö ohjelmoinnissa ja se juontaa juurensa Unix-käyttöjärjestelmästä. Siinä missä Go-kielellä pystyy lukemaan komentoriviltä syötettyjä parametreja, toiset kielet kuten Python vaativat erillisen moduulin käyttämistä.

## Katso myös:

- [Go-kielen dokumentaatio](https://golang.org/doc/)
- [Linux-komentoriviparametrit](https://www.tutorialspoint.com/unix_commands/getopt.htm)