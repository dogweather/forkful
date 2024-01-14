---
title:                "Ruby: Konvertere en streng til små bokstaver"
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor
Å konvertere en streng til små bokstaver er en nyttig funksjon i Ruby-programmering, spesielt når du jobber med datainnsamling eller sammenligning av tekst. Ved å gjøre dette vil du kunne behandle data mer effektivt og nøyaktig.

## Slik gjør du det
For å konvertere en streng til små bokstaver i Ruby, kan du bruke metoden `.downcase`, som vil returnere en ny streng med alle bokstaver i små bokstaver.

```Ruby
streng = "DETTE ER EN STRENG"
puts streng.downcase 
# output: dette er en streng
```

Du kan også bruke `!`-operatøren etter metoden for å endre originalstrengen, istedenfor å lage en ny.

```Ruby
streng = "DETTE ER EN STRENG"
streng.downcase! 
puts streng
# output: dette er en streng
```

## Dypdykk
Når du bruker `.downcase`-metoden, vil den kun konvertere bokstavene i den engelske alfabetet. Hvis du jobber med språk som inneholder spesielle bokstaver, som for eksempel norsk, vil disse bokstavene fremdeles være i store bokstaver. For å kunne konvertere disse bokstavene, kan du bruke metoden `.unicode_normalize` etterfulgt av `.downcase` for å konvertere dem til små bokstaver.

```Ruby
streng = "DETTE ER EN STRENG MED Ø Æ Å"
streng = streng.unicode_normalize.downcase 
puts streng
# output: dette er en streng med ø æ å
```

## Se også
- [Ruby dokumentasjon om stringer](https://ruby-doc.org/core-3.0.2/String.html)
- [Hvordan sammenligne strenger i Ruby](https://github.com/norwegianbokmål/ruby-string-comparison-article)