---
title:                "Hitta längden på en sträng"
html_title:           "Arduino: Hitta längden på en sträng"
simple_title:         "Hitta längden på en sträng"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att hitta längden på en sträng innebär att räkna antalet tecken i den. Vi behöver det för att navigera, manipulera, och upprepa genom strängar i vår kod.

## Så här gör du:
I Kotlin hittar du längden på en sträng med `.length` egenskapen. Till exempel, låt oss prova med strängen "Hej Världen".

```Kotlin
fun main() {
    val greeting = "Hej Världen"
    println(greeting.length)
}
```
Detta skriver ut "11" eftersom strängen består av 11 tecken.

## Djupdykning
(1) Konceptet att hitta längden på en sträng har sina rötter från de tidigaste programmeringsspråken. (2) I vissa språk, till exempel C, krävde det att iterera genom strängen tills du nådde ett null-tecken. Men i Kotlin är det bara en egenskap som du kan komma åt direkt. (3) `.length` är faktiskt en Kotlin 'synthetic extension property' på JVM:ens `java.lang.String.length()`, vilket innebär att det enkelt kan komma åt antalet Unicode-tecken i strängen.

## Se även
För mer information och exempel rekommenderar vi att du kollar på följande resurser:
- Kotlin Dokumentation om strängar: [Sträng i Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- In-depth on `.length`: [`.length` in Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/length.html)
- Video Tutorial om strängmanipulation i Kotlin: [Strängmanipulation i Kotlin](https://www.youtube.com/watch?v=M0bPxyRo4Vg)