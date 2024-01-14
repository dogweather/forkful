---
title:    "Kotlin: Utskrift av feilsøkingsutdata"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Hvorfor

Hvis du noen gang har programmert i Kotlin, har du sannsynligvis støtt på feilsøkingsproblemer. Enten det er en feil i koden din eller et problem med en tredjeparts bibliotek, er det alltid viktig å kunne finne og rette opp feil raskt. En effektiv måte å gjøre dette på er å bruke utdata for feilsøking, også kjent som debugging output. Det gir deg enkel tilgang til informasjon om variabler, funksjoner og andre viktige data når programmet ditt kjører. I denne bloggposten vil vi se på hvordan du kan bruke utdata for feilsøking i Kotlin for å forbedre programmets effektivitet og feilsøkingsprosess.

## Hvordan

Å legge til utdata for feilsøking i Kotlin er en ganske enkel prosess. Du kan bruke funksjonen "print()" eller "println()" for å skrive ut en tekststreng eller variabel i konsollen. La oss si at vi har en enkel funksjon som skal legge sammen to tall og returnere summen:

```Kotlin
fun addNumbers(a: Int, b: Int): Int {
    print("Tallene som blir lagt sammen er $a og $b")
    return a + b
}

val sum = addNumbers(5, 10)
println("Summen er $sum") 
```

I dette eksempelet brukte vi "print()" for å skrive ut informasjonen om tallene som blir lagt sammen. Deretter brukte vi "println()" for å skrive ut resultatet av funksjonen vår. Output i konsollen blir:

```
Tallene som blir lagt sammen er 5 og 10
Summen er 15
```

Å bruke utdata for feilsøking i Kotlin kan hjelpe deg med å forstå hva som skjer i programmet ditt i sanntid. Du kan også bruke "print()" og "println()" til å evaluere om en bestemt blokk med kode blir kjørt eller ikke. Dette er spesielt nyttig når du jobber med større prosjekter med mange klasser og funksjoner.

## Dypdykk

I tillegg til å bruke "print()" og "println()", kan du også bruke Kotlin sin innebygde funksjon "println()" for å skrive ut komplekse datastrukturer som lister, arrays og klasser. Dette er en flott måte å inspisere dataene dine på i sanntid og sjekke om alt fungerer som det skal.

En annen nyttig funksjon i Kotlin er "debug". Dette lar deg sette en breakpoint i koden din og gå inn i en modus som heter "debug mode". Mens du er i debug mode, kan du steg-for-steg gå gjennom koden og sjekke verdien på variabler og evaluere om betingelsene dine er sanne eller ikke. Dette er en effektiv måte å finne og rette feil på i koden din.

## Se også

- [Offisiell Kotlin dokumentasjon om debugging](https://kotlinlang.org/docs/tutorials/debugging.html)
- [10 tips for debugging med Kotlin](https://fasterjava.blogspot.com/2020/05/10-tips-for-debugging-in-kotlin.html)
- [The Ultimate Guide to Debugging Kotlin Code](https://blog.philipphauer.de/debugging-kotlin-code/)