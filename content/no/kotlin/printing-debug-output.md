---
title:    "Kotlin: Utskrift av feilsøkningsutdata"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

# Hvorfor
Feilsøking er en viktig del av enhver programmerers arbeidsflyt. Å kunne skrive ut feilmeldinger og variabelverdier for å forstå hva som skjer i koden din er en nyttig ferdighet i Kotlin-programmering. 

# Slik gjør du det
For å skrive ut feilmeldinger og variabelverdier, kan du bruke Kotlin sin innebygde funksjon 'println()'. Dette er en enkel måte å skrive ut ønsket informasjon på konsollen. 

```Kotlin 
fun main() {
   val navn = "Sara"
   println("Hei $navn, velkommen til Kotlin!")
}
```

Output: Hei Sara, velkommen til Kotlin! 

Du kan også bruke 'print()' hvis du bare vil skrive ut en enkel setning eller tekst uten å legge til en ny linje. 

```Kotlin 
fun main() {
   print("Dette er et eksempel på debug output.")
   print(" Dette vil bli skrevet ut på samme linje.")
}
```

Output: Dette er et eksempel på debug output. Dette vil bli skrevet ut på samme linje. 

# Dypdykk
I tillegg til å skrive ut enkle verdier og tekst, kan du også skrive ut mer kompleks informasjon som liste- og mappelementer. For dette kan du bruke 'println()' kombinert med Kotlin sin 'joinToString()' funksjon. Denne funksjonen konverterer liste- og mappelementer til en streng og gir deg muligheten til å legge til spesifikke separatorer og prefikser. 

```Kotlin 
fun main() {
   val mittFavorittall = listOf(3, 7, 9)
   println("Mitt favorittall er: ${mittFavorittall.joinToString(separator = ", ", prefix = "[", postfix = "]")}")   
}
```

Output: Mitt favorittall er: [3, 7, 9] 

# Se også
* [Kotlin dokumentasjon om debugging](https://kotlinlang.org/docs/tutorials/debugging-ide.html)
* [YouTube video om Kotlin feilsøking](https://www.youtube.com/watch?v=6D6yT456IfI)
* [10 nyttige tips for feilsøking i Kotlin](https://medium.com/better-programming/10-tips-to-handle-bugs-in-kotlin-a1168c4be20d)