---
title:    "Kotlin: Å finne lengden av en streng"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Hvorfor

Når du jobber med Kotlin-programmering, vil du ofte støte på situasjoner der du trenger å finne lengden av en streng. Det kan være for å validere brukerinput, formatere utdata eller manipulere data. Uansett grunn, er det viktig å kunne finne lengden av en streng på en effektiv måte.

# Hvordan

Det er flere måter å finne lengden av en streng i Kotlin, avhengig av dine behov og preferanser. En enkel og vanlig metode er å bruke Kotlin sin innebygde funksjon `length()`, som returnerer lengden av strengen. Se eksemplet nedenfor:

```Kotlin
val streng = "Hei, verden!"
println(streng.length()) // Output: 12
```

Dette fungerer godt for de fleste tilfeller, men det er også andre måter å finne strengens lengde på. Hvis du ønsker å ekskludere mellomrom fra tellingen, kan du bruke funksjonen `trim().length`, slik:

```Kotlin
val streng = " Hei, verden! "
println(streng.trim().length) // Output: 12
```

Hvis du ønsker å finne lengden av en streng i Unicode-tegn, kan du bruke funksjonen `codePointCount()` i stedet. Se eksemplet nedenfor:

```Kotlin
val streng = "Gårdsbruk 🚜"
println(streng.codePointCount(0, streng.length)) // Output: 11
```

Som du kan se, er det flere måter å finne strengens lengde på i Kotlin, så det er viktig å velge den som passer best til ditt spesifikke behov.

# Dykk dypere

For å få en enda bedre forståelse av hvordan Kotlin håndterer strenglengde, kan det være nyttig å se nærmere på hvordan `length()`-funksjonen fungerer. I motsetning til Java, der `length()` utelukkende tar hensyn til antall tegn, tar Kotlin også hensyn til Unicode-tegn og multi-byte-tegn.

I tillegg kan det være lurt å være oppmerksom på eventuelle forskjeller mellom `length()` og `codePointCount()`-funksjonen. `length()` returnerer antall tegn, mens `codePointCount()` returnerer antall Unicode-tegn, uavhengig av antall tegn.

# Se også

- [Offisiell Kotlin dokumentasjon om strenger](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Stack Overflow: Hvordan finne lengden av en streng i Kotlin](https://stackoverflow.com/questions/37695017/kotlin-find-length-of-string)
- [Kotlin for Android-utviklere kurs](https://developer.android.com/kotlin/courses)