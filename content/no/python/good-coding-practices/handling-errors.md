---
date: 2024-01-26 00:56:41.820912-07:00
description: "Feilh\xE5ndtering i Python (eller ethvert programmeringsspr\xE5k) handler\
  \ om \xE5 forvente det uventede \u2013 det er kunsten \xE5 h\xE5ndtere n\xE5dige\
  \ problemer n\xE5r ting g\xE5r\u2026"
lastmod: '2024-03-13T22:44:40.368824-06:00'
model: gpt-4-1106-preview
summary: "Feilh\xE5ndtering i Python (eller ethvert programmeringsspr\xE5k) handler\
  \ om \xE5 forvente det uventede \u2013 det er kunsten \xE5 h\xE5ndtere n\xE5dige\
  \ problemer n\xE5r ting g\xE5r galt i koden din."
title: "Feilh\xE5ndtering"
weight: 16
---

## Hva & Hvorfor?

Feilhåndtering i Python (eller ethvert programmeringsspråk) handler om å forvente det uventede – det er kunsten å håndtere nådige problemer når ting går galt i koden din. Vi gjør det for å forhindre krasj, veilede brukere, og gjøre programmene våre robuste og pålitelige.

## Hvordan gjøre:

``` Python
# Grunnleggende try-except-blokk
try:
    # risikofylt kode
    tall = int(input("Skriv inn et tall: "))
except ValueError:
    # håndtere feil
    print("Det er ikke et tall!")

# Spesifisere flere unntak
try:
    # kode som kan utløse forskjellige unntak
    resultat = 10 / int(input("Skriv inn en divisor: "))
except ZeroDivisionError:
    print("Oops! Kan ikke dele på null.")
except ValueError:
    print("Jeg trenger et tall, kompis.")

# Bruke else og finally
try:
    tall = int(input("Skriv inn et tall for kvadrering: "))
except ValueError:
    print("Jeg sa et tall!")
else:
    # ingen feil oppstått
    print("Ditt tall kvadrert er:", tall**2)
finally:
    # utfører alltid
    print("Takk for at du prøvde dette ut!")
```

Eksempel på utdata når du skriver inn et ugyldig tall for den første blokken:
```
Skriv inn et tall: hallo
Det er ikke et tall!
```

## Dypdykk

Siden programmeringens begynnelse har feilhåndtering vært avgjørende. Tidlige tilnærminger var rudimentære, som å sjekke forhold før hver risikofylt operasjon. Pythons `try-except`-syntaks kommer fra en arv av unntakshåndtering i eldre språk som C++ og Java, noe som forenkler prosessen.

Når du `try` (forsøker) en blokk med kode, ser Python etter eventuelle unntak. Hvis en feil dukker opp, fanger `except`-blokken den. Du kan bli spesifikk om unntakene du fanger, eller fange alle med et enkelt `except`. Men, det er bedre å være spesifikk først – det er presist, ikke et fangstnett for alt.

`else` og `finally` er ekstra i dette konseptet. `else`-blokken kjører hvis try-blokken er feilfri. `finally` er den pålitelige kameraten som kjører uansett hva som skjer – tenk oppryddingsoperasjoner.

Alternativer? De finnes helt sikkert. Noen språk bruker returkoder i stedet for unntak. Du kan også møte på `with`-setninger for håndtering av ressurser, eller `assertions` som sjekker forhold under utvikling. Men når vi snakker om solide feilhåndteringsstrategier, skiller try-catch-modellen seg ut for sin lesbarhet og struktur.

## Se også

Her er noen gode ekstra ressurser for å dykke enda dypere:

- Pythons offisielle dokumentasjon om feil og unntak: [Python Docs – Errors and Exceptions](https://docs.python.org/3/tutorial/errors.html)
- Real Pythons guide om emnet: [Real Python - The try/except/else/finally block](https://realpython.com/python-exceptions/)
- En gjennomtenkt diskusjon om beste praksis for feilhåndtering: [Stack Overflow – How do I properly ignore exceptions?](https://stackoverflow.com/questions/4990718/about-catching-any-exception)
