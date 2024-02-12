---
title:                "Willekeurige getallen genereren"
aliases: - /nl/kotlin/generating-random-numbers.md
date:                  2024-01-28T22:00:45.536174-07:00
model:                 gpt-4-0125-preview
simple_title:         "Willekeurige getallen genereren"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/kotlin/generating-random-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Het genereren van willekeurige getallen in programmering gaat over het creëren van getallen die geen enkel voorspelbaar patroon hebben. Programmeurs doen dit om verschillende redenen, waaronder simulaties, het testen van algoritmes, gaming en veiligheidstoepassingen, waar onvoorspelbaarheid cruciaal is om realistische of veilige resultaten te bereiken.

## Hoe:

Kotlin biedt een eenvoudige manier om willekeurige getallen te genereren via zijn standaardbibliotheek. Hier is hoe je verschillende soorten willekeurige waarden kunt genereren:

### Een Willekeurig Geheel Getal Genereren

Om een willekeurig geheel getal binnen een specifiek bereik te genereren:

```kotlin
import kotlin.random.Random

fun main() {
    val randomNumber = Random.nextInt(1, 100) // Genereert een willekeurig getal tussen 1 en 99
    println(randomNumber)
}
```

### Een Willekeurige Double Genereren

Op een vergelijkbare manier, een willekeurige double genereren:

```kotlin
import kotlin.random.Random

fun main() {
    val randomDouble = Random.nextDouble(1.0, 10.0) // Genereert een willekeurige double tussen 1.0 en 10.0
    println(randomDouble)
}
```

### Een Willekeurige Boolean Genereren

Om een willekeurige booleanwaarde te genereren:

```kotlin
import kotlin.random.Random

fun main() {
    val randomBoolean = Random.nextBoolean() // Genereert willekeurig ofwel true of false
    println(randomBoolean)
}
```

### Seeding voor Reproduceerbare Resultaten

In gevallen waar je reproduceerbare reeksen van willekeurige getallen nodig hebt (bijvoorbeeld, in testen), kun je de willekeurige getallengenerator seeden:

```kotlin
import kotlin.random.Random

fun main() {
    val seed = 12345L
    val random = Random(seed)
    val randomNumber = random.nextInt(1, 100)
    println(randomNumber)
}
```

## Diepgaand

De benadering van de Kotlin-standaardbibliotheek voor het genereren van willekeurige getallen maakt onder de motorkap gebruik van Java's `java.util.Random`, wat zorgt voor een combinatie van gebruiksgemak en prestaties. Het is echter cruciaal om op te merken dat deze methoden pseudo-willekeurige getallen genereren, wat betekent dat de getallen willekeurig lijken, maar worden gegenereerd met behulp van een deterministisch proces.

Voor de meeste toepassingen is de willekeurigheid die door Kotlin's `Random`-klasse wordt geboden, voldoende. Echter, voor meer beveiligingsgevoelige toepassingen, zoals cryptografie, waar de kwaliteit van willekeurigheid van het grootste belang is, zou men moeten overwegen om `java.security.SecureRandom` te gebruiken in plaats daarvan. SecureRandom is specifiek ontworpen voor cryptografische operaties en biedt een hogere kwaliteit van willekeurigheid, zij het met een mogelijke afweging in prestaties.

Kotlin heruitvindt het wiel niet maar biedt een Kotlin-vriendelijke API over Java's mechanismen voor het genereren van willekeurige getallen, waardoor het idiomatischer en bondiger te gebruiken is binnen Kotlin-projecten. Zoals altijd, bij het omgaan met willekeurigheid, moeten programmeurs zorgvuldig de use case overwegen om het meest geschikte gereedschap voor de taak te kiezen.
