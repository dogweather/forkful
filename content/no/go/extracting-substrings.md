---
title:                "Utvinning av substrenger"
html_title:           "Go: Utvinning av substrenger"
simple_title:         "Utvinning av substrenger"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor vil noen ønske å ekstrahere substringer i en Go-programmeringskode? Vel, det kan være flere grunner til det. Noen ganger trenger vi bare en del av en tekststreng for å bruke det til en bestemt oppgave. Eller kanskje vi ønsker å utføre operasjoner på deler av en tekststreng uten å endre den originale strengen. Uansett hva årsaken måtte være, er det viktig å vite hvordan man ekstraherer substringer i Go for å gjøre koden mer effektiv og hendig.

## Hvordan

For å ekstrahere substringer i Go, kan vi bruke den innebygde funksjonen "substring()" som tar inn en startindeks og lengden på ønsket substring. La oss se på et eksempel:

```Go
str := "Hei, dette er en tekststreng"
sub := str[4:11] // Denne koden vil ekstrahere "dette er" fra den opprinnelige teksten

fmt.Println(sub)
```

Output:
```Go
dette er
```

Vi kan også bruke funksjonen "len()" for å finne lengden på en tekststreng, og dermed kan vi ekstrahere en substring basert på dette. La oss ta et annet eksempel:

```Go
str := "Dette er en lang tekststreng"
sub := str[4:len(str)-7] // Denne koden vil ekstrahere "er en lang tekst"

fmt.Println(sub)
```

Output:
```Go
er en lang tekst
```

En annen metode for å ekstrahere substringer er ved hjelp av funksjonen "strings.Split()" som deler en tekststreng basert på et gitt tegn eller delstreng. La oss se et eksempel:

```Go
str := "Dette,er,en,tekststreng"
sub := strings.Split(str, ",") // Denne koden vil returnere en liste med substrings ("Dette", "er", "en", "tekststreng")

fmt.Println(sub)
```

Output:
```Go
[Dette er en tekststreng]
```

## Dypdykk

Å ekstrahere substringer kan også gjøres ved hjelp av Regular Expressions (regex) i Go. Dette er en kraftig metode for å søke etter spesifikke mønstre i en tekststreng og ekstrahere de ønskede delene. For å bruke regex i Go, må vi importere "regexp" pakken og deretter kan vi bruke funksjonen "FindStringSubmatch()" for å ekstrahere substrings basert på et regex-mønster. La oss se på et eksempel:

```Go
str := "Dette er en tekststreng 12345"
regex := regexp.MustCompile(`(\w+) (\d+)`) // regex mønsteret som brukes for å ekstrahere tekst før og etter tallet
substr := regex.FindStringSubmatch(str)

fmt.Println(substr[0]) // Denne koden vil ekstrahere "Dette er en tekststreng 12345" fra den opprinnelige teksten
fmt.Println(substr[1]) // Denne koden vil ekstrahere "Dette er en tekststreng" 
fmt.Println(substr[2]) // Denne koden vil ekstrahere "12345"
```

Output:
```Go
Dette er en tekststreng 12345
Dette er en tekststreng
12345
```

Som du kan se, er det flere måter å ekstrahere substringer i Go, avhengig av dine behov og preferanser. Ved å bruke substrings riktig kan vi gjøre koden mer effektiv og lesbar.

## Se også

- [Strings pakken i Go](https://golang.org/pkg/strings/)
- [Regexp pakken i Go](https://golang.org/pkg/regexp/)
- [Go regex tutorial](https://gobyexample.com/regular-expressions)