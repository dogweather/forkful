---
title:                "Skriver til standard feil"
html_title:           "Haskell: Skriver til standard feil"
simple_title:         "Skriver til standard feil"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?

Skriving til standard error er en måte å sende feilmeldinger og annen informasjon til programmerere under kjøring av et program. Dette er nyttig når du vil finne og fikse feil i koden din. Programmere bruker dette for å få mer informasjon om hva som skjer når et program kjører, slik at de kan forbedre det.

# Hvordan gjøre det:

Kodingseksempler nedenfor viser hvordan du skriver til standard error i Haskell:

```Haskell 
main = do
  putStrLn "Dette er en vanlig utskrift"
  hPutStrLn stderr "Dette er en feilmelding"
```

Eksempelet ovenfor vil skrive ut "Dette er en vanlig utskrift" til standard output, men "Dette er en feilmelding" vil bli skrevet til standard error. Denne metoden kan også brukes til å printe ut variabler eller feilmeldinger.

# Dykk dypere:

I tidligere versjoner av Haskell, måtte man bruke en annen funksjon kalt "hGetErrorStream" for å skrive til standard error. Dette ble forbedret i nyere versjoner, som nå tillater bruk av "hPutStrLn". Det finnes også andre måter å sende feilmeldinger til programmerere, som for eksempel å skrive dem til en fil eller sende dem som e-post.

I tillegg, hvis du vil skrive til både standard output og standard error samtidig, kan du bruke "hPutStr" istedenfor "hPutStrLn", slik at det ikke blir en linjeskift mellom utskriftene.

# Se også:

For mer informasjon og eksempler, se følgende ressurser:

- [Haskell dokumentasjon om I/O](https://www.haskell.org/documentation/)