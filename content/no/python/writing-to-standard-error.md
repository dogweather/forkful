---
title:                "Python: Skriving til standardfeil"
simple_title:         "Skriving til standardfeil"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor

Skriver du programmer på Python og lurer på om du skal skrive til standardfeil? Da er dette innlegget for deg! Å skrive til standardfeil er en viktig måte å håndtere feil i programmene dine på, og kan hjelpe deg med å finne og fikse potensielle problemer i koden din.

## Hvordan

```Python
try:
    # noe kode som kan potensielt krasje
except Exception as e:
    # skriv til standardfeil med feilmeldingen
    print("Noe gikk galt: ", e, file=sys.stderr)
```

I dette eksempelet har vi en ```try```-blokk som inneholder kode som kan potensielt krasje. Hvis en feil oppstår, vil den bli fanget i ```except```-blokken og skrevet ut til standardfeil ved hjelp av ```file=sys.stderr```. Dette vil gi en mer beskrivende feilmelding enn bare å la programmet krasje.

### Feilmeldinger og debugging

Når du skriver til standardfeil, vil feilmeldinger vises i consolen når du kjører programmet ditt. Dette gjør det enklere å finne og fikse feil i koden din. Ved å skrive til standardfeil med en beskrivende melding, kan du få mer informasjon om hvor og hvorfor feilen oppstår.

## Dypdykk

Når du skriver til standardfeil, kan du også formatere feilmeldingen din for å gjøre den enda mer nyttig. For eksempel kan du legge til ekstra informasjon som linjenummer og filnavn der feilen oppstår. Dette gjør det lettere å finne og fikse feilen i koden din.

## Se også

- [Python dokumentasjon om feilhåndtering](https://docs.python.org/3/tutorial/errors.html)
- [Blogginnlegg om debugging i Python](https://realpython.com/python-debugging-pdb/) 
- [YouTube video om skriving til standardfeil på Python](https://www.youtube.com/watch?v=hFPeoCOfsJw)