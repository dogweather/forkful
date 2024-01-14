---
title:    "Kotlin: Omvandla ett datum till en sträng"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

# Varför
Att kunna konvertera ett datum till en sträng är en viktig funktion i Kotlin-programmering eftersom det gör det möjligt att visa datumet på ett läsbart format för användaren eller att använda det i andra delar av koden.

# Hur man gör det
Det första som behövs är att skapa ett datumobjekt med hjälp av Kotlin's Date-klass.

```Kotlin
val datum: Date = Date()
```

Sedan kan vi använda Kotlin's SimpleDateFormat-klass för att konvertera datumet till en sträng. Här är ett exempel där vi konverterar datumet till formatet "dd.MM.yyyy".

```Kotlin
val format: SimpleDateFormat = SimpleDateFormat("dd.MM.yyyy")
val datumStrang: String = format.format(datum)
println(datumStrang) // Output: 23.08.2021
```

Vi kan också inkludera tiden i vår sträng genom att lägga till ett tidsformat, till exempel "dd.MM.yyyy HH:mm:ss". Detta kommer att ge oss en sträng som inkluderar både datum och tid.

```Kotlin
val format: SimpleDateFormat = SimpleDateFormat("dd.MM.yyyy HH:mm:ss")
val datumStrang: String = format.format(datum)
println(datumStrang) // Output: 23.08.2021 12:34:56
```

# Djupdykning
När vi använder SimpleDateFormat-klassen är det viktigt att veta vilka bokstäver som ska användas för att få det önskade formatet. Här är en lista över de vanligaste bokstäverna och vad de står för:

- d: dag i månaden (1-31)
- M: månad (1-12)
- y: år (exempelvis 2021)
- H: timme (0-23)
- m: minut (0-59)
- s: sekund (0-59)

Det finns även möjlighet att inkludera fler tecken för att formatera datumet och tiden på olika sätt. Mer information om detta, och andra användbara funktioner, hittar du i Kotlin's Date- & SimpleDateFormat-dokumentation.

# Se även
- [Kotlin's Date-klass](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-date/)
- [Kotlin's SimpleDateFormat-klass](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-simple-date-format/index.html)