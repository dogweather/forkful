---
title:                "Fjerne anførselstegn fra en streng"
aliases: - /no/kotlin/removing-quotes-from-a-string.md
date:                  2024-01-26T03:40:37.499855-07:00
model:                 gpt-4-0125-preview
simple_title:         "Fjerne anførselstegn fra en streng"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å fjerne anførselstegn fra en streng betyr å strippe bort eventuelle forekomster av anførselstegn, enten enkle (' ') eller doble (" "), fra tekstdataene du jobber med. Programmerere trenger ofte å gjøre dette for datarensing, for å forberede videre behandling, eller når anførselstegnene selv ikke er relevante for dataenes mening.

## Hvordan:

Her er en enkel måte å fjerne begge typer anførselstegn fra en streng i Kotlin:

```kotlin
fun removeQuotes(input: String): String {
    return input.replace("\"", "").replace("'", "")
}

fun main() {
    val stringWithQuotes = "Kotlin \"rocks\" it's 'cool'"
    val stringWithoutQuotes = removeQuotes(stringWithQuotes)
    println(stringWithoutQuotes) // Output: Kotlin rocks its cool
}
```

Og hvis du vil fjerne bare én type anførselstegn, kan du bare hoppe over den andre erstatningskallet.

```kotlin
fun removeDoubleQuotes(input: String): String {
    return input.replace("\"", "")
}

fun removeSingleQuotes(input: String): String {
    return input.replace("'", "")
}

fun main() {
    val stringWithQuotes = "Kotlin \"rocks\" it's 'cool'"
    println(removeDoubleQuotes(stringWithQuotes)) // Output: Kotlin rocks it's 'cool'
    println(removeSingleQuotes(stringWithQuotes)) // Output: Kotlin "rocks" its cool
}
```

## Dypdykk

Historisk sett har håndtering av strenger og unngåelse av tegn vært en grunnleggende del av programmering, da tekst er en grunnleggende måte vi samhandler med data på. Anførselstegn inni strenger trenger noen ganger å unngås. Dette indikeres ved et forutgående skråstrek (f.eks., `"Hun sa, \"Hei!\""`). Når du behandler slike strenger, kan du trenge å fjerne unngåelses tegnene, eller selve anførselstegnene for renere eller mer brukbar tekst.

Alternativer til `replace`-metoden inkluderer fjerning basert på regulære uttrykk eller manuell parsing av strengen, tegn for tegn. Imidlertid kan regulære uttrykk være overkill for enkle operasjoner, og manuell parsing er mindre effektiv enn å bruke innebygde strengfunksjoner. Kotlins `replace`-funksjon benytter seg av den underliggende Javas `String` `replace`-metoden, som er godt optimalisert for ytelse.

Når det gjelder implementering, er det verdt å nevne at Kotlin er samvirkebar med Java, så, i effekt, er alle operasjoner du utfører på strenger så ytelsesrike som de ville vært i Java. Det er avgjørende når du fjerner anførselstegn å være oppmerksom på kanttilfeller, som innebygde anførselstegn, som kunne kreve en mer sofistikert tilnærming, muligens ved å benytte regulære uttrykk eller et parserbibliotek.

## Se også

For mer kontekst om håndtering av strenger i Kotlin, kan du sjekke ut den offisielle dokumentasjonen:

- [Kotlins strengdokumentasjon](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)

For dypere dykk i regulære uttrykk og parsing i Kotlin:

- [Kotlin Regex-dokumentasjon](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
