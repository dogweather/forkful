---
title:    "Haskell: Utskrift av feilrettingsutdata"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Hvorfor

Mange ganger når vi koder i Haskell, kan det være nyttig å ha utskrift av feilsøkingsinformasjon mens vi utvikler og tester koden vår. Dette kan hjelpe oss med å forstå hva som skjer i programmet vårt og identifisere eventuelle problemer.

## Hvordan gjør man det

For å få utskrift av feilsøkingsinformasjon i Haskell, kan vi bruke `Debug.Trace` -modulen. Denne modulen inneholder en funksjon som heter `trace`, som lar oss skrive ut en verdi og en tilhørende beskjed.

```Haskell
import Debug.Trace

-- Definerer en funksjon som legger sammen to tall og skriver ut en feilsøkingsmelding
add :: Int -> Int -> Int
add x y = trace ("Legger sammen " ++ show x ++ " og " ++ show y) (x + y)

-- Kaller på funksjonen og lagrer resultatet i en variabel
resultat = add 5 7

-- Skriver ut resultatet
print resultat
```

Dette vil gi oss utskriften:

```
Legger sammen 5 og 7
12
```

Vi kan også bruke denne funksjonen til å skrive ut verdiene til variabler inne i en funksjon for å se hvordan de endrer seg. La oss se på et eksempel:

```Haskell
import Debug.Trace

-- Enkel funksjon som returnerer resultatet av å multiplisere to tall
multiply :: Int -> Int -> Int
multiply x y = trace ("Multipliserer " ++ show x ++ " og " ++ show y) (x * y)

-- Definerer en variabel og bruker funksjonen
resultat = multiply 3 4

-- Skriver ut variabelen
print resultat
```

Vi får da følgende utskrift:

```
Multipliserer 3 og 4
12
```

Som du kan se, har vi fått en feilsøkingsmelding som forteller oss hva som skjer inne i funksjonen, og så får vi resultatet av utregningen.

## Dykk dypere

Det er viktig å merke seg at `trace`-funksjonen bare er ment for feilsøking og ikke skal brukes i faktisk produksjonskode. Det er fordi denne funksjonen faktisk endrer koden vår og legger til ekstra utskrift som ikke er nødvendig for å kjøre programmet. Derfor bør vi sørge for å fjerne alle `trace`-funksjoner før vi deployer koden vår.

Vi kan også bruke `trace`-funksjonen sammen med typen `IO` for å skrive ut feilsøkingsmeldinger i IO-operasjoner. Dette kan være spesielt nyttig hvis vi har et program som leser eller skriver til filer eller gjør andre IO-relaterte operasjoner.

## Se også

- [GHCi user's guide for trace](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html#using-trace)
- [Debugging Haskell code with trace and GHCi](https://blog.haskellforall.com/2016/04/27/debugging-haskell-code-with-trace-and-ghci/)