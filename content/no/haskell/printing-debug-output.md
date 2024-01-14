---
title:                "Haskell: Utskrift av feilsøkingsutdata"
programming_language: "Haskell"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor

Vi har alle vært der - prøver å løse en feil i koden vår, men støter på en murvegg. Vi prøver å lese koden og forstå hva som skjer, men ingenting virker galt. Det er her debugging kommer inn i bildet. Den enkleste måten å få en bedre forståelse av hva som skjer i koden vår er å legge inn debug utskrifter, som printer verdiene av variabler og uttrykk. Dette kan hjelpe oss med å finne feil og forbedre vår generelle forståelse av koden.

## Hvordan

For å skrive ut debug-utskrifter i Haskell, bruker vi funksjonen `putStrLn`. Vi kan inkludere denne funksjonen i hvilken som helst del av koden vår, og den vil skrive ut en string på konsollen når programmet kjører. La oss ta en titt på et eksempel:

```Haskell
main :: IO ()
main = do
  let tall = 42
  putStrLn ("Tallet er: " ++ show tall)
```

I dette eksempelet lager vi en variabel `tall` med verdien `42`. Vi bruker deretter `putStrLn` til å skrive ut en string som inkluderer verdien av `tall`. Når vi kjører programmet, blir følgende skrevet ut på konsollen:

```
Tallet er: 42
```

Vi kan også legge til flere debug-utskrifter for å se verdien av flere variabler og uttrykk. For å gjøre dette, må vi bare legge til flere `putStrLn`-kall i koden vår. Det kan også være nyttig å inkludere en beskrivende tekst for å vite hva som blir skrevet ut.

```Haskell
main :: IO ()
main = do
  let tall1 = 12
      tall2 = 30
  putStrLn "Debug-utskrifter:"
  putStrLn ("Tall 1: " ++ show tall1)
  putStrLn ("Tall 2: " ++ show tall2)
  putStrLn ("Summen av tall 1 og tall 2: " ++ show (tall1 + tall2))
```

Dette vil resultere i følgende utskrift:

```
Debug-utskrifter:
Tall 1: 12
Tall 2: 30
Summen av tall 1 og tall 2: 42
```

## Dypdykk

Det er viktig å merke seg at debugging ikke er en erstatning for god koding. Det er bare et nyttig verktøy for å hjelpe oss med å forstå hva som skjer bak kulissene i koden vår. Det er også viktig å huske å fjerne alle debug-utskrifter før du deployer koden din til produksjon.

En annen nyttig funksjon for debugging i Haskell er `trace`. Denne funksjonen tar en string og et uttrykk som argumenter, og evaluerer uttrykket mens den skriver ut stringen på konsollen. La oss se på et eksempel:

```Haskell
sum :: Num a => [a] -> a
sum []     = 0
sum (x:xs) = trace ("Legger til " ++ show x) x + sum xs
```

Her bruker vi funksjonen `trace` for å skrive ut en string som forteller oss hva som blir lagt til i summen hver gang funksjonen blir kalt. Dette kan være spesielt nyttig når du jobber med rekursive funksjoner.

## Se også

- [Debugging i Haskell (engelsk)](https://wiki.haskell.org/Debugging)
- [Feilsøkingstips for Haskell (engelsk)](https://www.yesodweb.com/blog/2012/01/tips-for-debugging-haskell)