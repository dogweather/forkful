---
title:                "Utskrift av feilsøkingsresultater"
html_title:           "Haskell: Utskrift av feilsøkingsresultater"
simple_title:         "Utskrift av feilsøkingsresultater"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hva & hvorfor?

Når vi koder, er det ofte nyttig å kunne se hva som skjer i programmet vårt i sanntid. Det kalles å printe ut feilsøkingsutdata (debug output), og det gjør programmererne for å forstå hvordan programmet fungerer og finne eventuelle feil.

## Slik gjør du det:

```Haskell 
-- Definer en funksjon som printer ut en tekststreng
printTekst :: String -> IO ()
printTekst tekst = putStrLn tekst

-- Kjør funksjonen med en tekststreng som argument
main = do
  printTekst "Hei, verden!"
```

Output:

```Haskell 
Hei, verden!
```

## Dykk dypere:

Det å printe ut debug output har vært en del av programmering helt siden begynnelsen. Alternativene til å printe ut informasjon er å bruke en debugger eller å bruke assert uttrykk for å sjekke tilstanden av programmet på et visst punkt. Men å printe ut feilsøkingsutdata er ofte den enkleste og mest effektive måten å få en oversikt over hva som skjer i programmet vårt.

## Se også:

- [Debugging Techniques for Functional Programming Languages](https://www.cs.kuleuven.be/~gerda/Teaching/AP/08-09/201/slides/ap-W09.pdf)
- [Debugging i Haskell ved bruk av print](https://wiki.haskell.org/Debugging)
- [Haskell Debugging Tools](https://hackage.haskell.org/package/haskell-debug)