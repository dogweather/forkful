---
title:                "Clojure: Søking og erstattning av tekst"
simple_title:         "Søking og erstattning av tekst"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor

Mange av oss har sittet fast i en tekstbehandler og endret samme ord eller setning flere ganger i en lang tekst. Dette kan være tidkrevende og kjedelig arbeid. Ved å bruke Clojure, kan du enkelt automatisere dette ved å søke og erstatte tekst. Dette vil spare deg for mye tid og gjøre programmeringsoppgaver mer effektive.

## Hvordan

For å søke og erstatte tekst i Clojure, kan du bruke funksjonen `clojure.string/replace`. Denne funksjonen tar tre argumenter: den opprinnelige teksten, søkeordet og erstatningsordet. La oss si at du ønsker å endre alle forekomster av "hallo" til "hei" i en tekst. Da kan du bruke følgende kode:

```Clojure
(clojure.string/replace "Hallo, verden!" "hallo" "hei")
```

Dette vil returnere en ny tekst "Hei, verden!". Du kan også bruke regulære uttrykk i stedet for å spesifisere et enkelt søkeord. For eksempel:

```Clojure
(clojure.string/replace "123bogus45" #"[a-zA-Z]+" " ")
```

Dette vil fjerne alle bokstaver og returnere "123 45". Hvis du ønsker å erstatte teksten i en fil, kan du bruke `spit`-funksjonen til å skrive ut teksten til en ny fil. For eksempel:

```Clojure
(spit "ny_fil.txt" (clojure.string/replace "gammel_fil.txt" "hallo" "hei"))
```

Dette vil skrive den opprinnelige filen med alle forekomster av "hallo" erstattet med "hei".

## Dypdykk

I tillegg til `replace`-funksjonen, har Clojure også funksjonen `replace-first` som erstatter den første forekomsten av et søkeord med et erstatningsord. Du kan også bruke `replace`-funksjonen med en funksjon som et argument for å tilpasse hvilken tekst som skal byttes ut. For eksempel:

```Clojure
(clojure.string/replace "1 2 3" #"[0-9]+"
  (fn [m] (str (Integer/parseInt m) "stall")))
```

Dette vil returnere "1stall 2stall 3stall". Du kan også bruke `replace-first` med en funksjon på samme måte.

## Se Også

- [Clojure.org](https://clojure.org/)
- [ClojureDocs](https://clojuredocs.org/)
- [Regular Expressions i Clojure](https://clojure.org/guides/regular_expressions)