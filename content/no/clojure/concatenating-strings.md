---
title:                "Sammenstillinger av strenger"
html_title:           "Clojure: Sammenstillinger av strenger"
simple_title:         "Sammenstillinger av strenger"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor

Har du noen gang ønsket å kombinere flere strenger for å lage en lengre og mer meningsfull setning? Vel, heldigvis gjør Clojure det enkelt å gjøre nettopp det. Ved å kombinere enkle strenger kan du skape mer komplekse uttrykk og løse problemer på en mer effektiv måte.

## Hvordan gjøre det

```Clojure
user=> (str "Jeg elsker" "å programmere" "i Clojure")
=> "Jeg elsker å programmere i Clojure"
```
Her har vi brukt funksjonen `str` til å kombinere tre forskjellige strenger. Dette gjøres ved å plassere dem etter hverandre som argumenter til `str`. Outputen er en ny streng som er sammensatt av de tre originale strengene.

Du kan også legge til andre typer data, som tall eller variabler:

```Clojure
user=> (def name "Maria")
=> #'user/name
user=> (str "Hei, mitt navn er" name)
=> "Hei, mitt navn er Maria"
```
Her har vi brukt variabelen `name` i kombinasjon med en streng. Dette viser hvordan du kan lage mer dynamiske setninger ved å inkludere variabelverdier.

En annen nyttig funksjon er `subs`, som lar deg hente ut en del av en streng basert på en startindeks og en sluttindeks:

```Clojure
user=> (subs "Clojure er et kraftig språk" 0 7)
=> "Clojure"
```
Denne koden vil returnere strengen som starter med indeks 0 (første bokstav) og slutter med indeks 7 (bokstav nummer 7). Dette er nyttig når du trenger å hente ut en del av en større streng.

## Deep Dive

Som nevnt tidligere, bruker vi funksjonen `str` for å kombinere strenger. Men hvordan fungerer det egentlig under panseret? Kort sagt konverterer `str` alle argumentene sine til strenger og slår dem sammen til én streng.

En annen viktig funksjon er `format`, som lar deg legge til variabler og formateringsregler:

```Clojure
user=> (def age 24)
=> #'user/age
user=> (format "Jeg er %d år gammel" age)
=> "Jeg er 24 år gammel"
```
Her har vi brukt formateringsregelen `%d`, som indikerer at du vil inkludere et heltall (age) i strengen. Du kan også bruke `%s` for å inkludere en streng og `%f` for å inkludere et desimaltall. Dette gir deg større kontroll over hvordan argumentene skal presenteres i den endelige strengen.

## Se også

- [Offisiell Clojure-dokumentasjon](https://clojure.org/)
- [Clojure spørsmål og svar på Stack Overflow](https://stackoverflow.com/questions/tagged/clojure)
- [En introduksjon til Clojure-programmering](https://www.baeldung.com/clojure-programming-introduction)