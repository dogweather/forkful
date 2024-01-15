---
title:                "Generering av tilfeldige tall"
html_title:           "Kotlin: Generering av tilfeldige tall"
simple_title:         "Generering av tilfeldige tall"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor generere tilfeldige tall? Det kan være mange grunner til å ønske å generere tilfeldige tall i programmering. Kanskje du skal lage et spill som trenger et tilfeldig tall for å bestemme neste trekk, eller kanskje du skal generere et tilfeldig passord for en bruker. Uansett hva grunnen måtte være, så er det viktig å vite hvordan man kan generere tilfeldige tall i Kotlin.

## Hvordan

Kotlin har en innebygd funksjon for å generere tilfeldige tall, kalt `Random()`. La oss se på et eksempel hvor vi skal generere et tilfeldig tall mellom 1 og 10:

```Kotlin
val tilfeldigTall = Random().nextInt(10) + 1
println(tilfeldigTall)
```

I dette tilfellet vil `tilfeldigTall` ha en verdi mellom 1 og 10. Vi bruker `nextInt()`-metoden fra `Random`-klassen til å generere et tall mellom 0 og 9, og så legger vi til 1 for å få et tall mellom 1 og 10.

Du kan også bruke `Random()`-funksjonen til å generere tilfeldige desimaltall. Her er et eksempel hvor vi skal generere et tilfeldig desimaltall mellom 0 og 1:

```Kotlin
val tilfeldigDesimal = Random().nextDouble()
println(tilfeldigDesimal)
```

I dette tilfellet vil `tilfeldigDesimal` ha en verdi mellom 0 og 1. `nextDouble()`-metoden fra `Random`-klassen generer en tilfeldig desimaltall mellom 0 og 1.

Det er også mulig å generere tilfeldige tall innenfor et spesifikt område. Her er et eksempel hvor vi skal generere et tilfeldig tall mellom 100 og 500:

```Kotlin
val tilfeldigTall = Random().nextInt(401) + 100
println(tilfeldigTall)
```

I dette tilfellet bruker vi `nextInt(max)`-metoden og legger på et tall til max-verdien for å definere et område.

## Deep Dive

Å generere tilfeldige tall er ikke alltid like lett som det kan virke. I programmering er det nemlig viktig at resultatene er så tilfeldige som mulig. Heldigvis tar `Random()`-funksjonen i Kotlin hensyn til dette og bruker avanserte algoritmer for å generere tilfeldige tall.

Det er viktig å være klar over at tilfeldige tall ikke er helt tilfeldige. De er basert på tall som kalles "seed" og det er denne seeden som bestemmer hvilke tilfeldige tall som skal genereres. Derfor er det viktig å alltid oppdatere seeden når man genererer tilfeldige tall, så man ikke ender opp med de samme tallene hver gang.

Man kan også bruke `setSeed(seed)`-metoden for å kontrollere seeden og dermed også kontrollere hvilke tilfeldige tall som blir generert.

## Se også

- [Offisiell Kotlin-dokumentasjon om Random](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/index.html)
- [Tutorial: Hvordan generere tilfeldige tall i Kotlin](https://www.geeksforgeeks.org/kotlin-random-class/)
- [Diskusjon om tilfeldige tall i Kotlin](https://stackoverflow.com/questions/45283797/is-random-in-kotlin-truly-randomized-what-are-the-hazards-of-using-random-insi)