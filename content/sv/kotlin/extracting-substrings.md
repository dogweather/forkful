---
title:    "Kotlin: Extrahera substrängar"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Varför

Att extrahera substrängar är en viktig färdighet inom programmering som kan hjälpa till att göra koden mer effektiv och lättare att förstå. Genom att lära sig denna teknik kan du enkelt utvinna specifika delar av en sträng som du behöver för ditt program. Detta kan vara till nytta för att göra diversa operationer inom en textbaserad applikation, som att hitta och ersätta ord eller extrahera information från en textfil.

## Så här gör du

För att extrahera en substräng från en befintlig sträng, använd `substring()`-funktionen i Kotlin. Denna funktion kräver två parametrar - startindex och slutindex - som anger vilka tecken från den ursprungliga strängen som ska extraheras.

```
Kotlin val originalStrang = "Det här är en teststräng"
val nyStrang = originalStrang.substring(4,8)
```

I detta exempel kommer `nyStrang` att innehålla ordet "är" från den ursprungliga strängen. Detta är eftersom vi har använt startindex 4 (starta efter "här") och slutindex 8 (sluta innan "teststräng").

Du kan också använda `substring()`-funktionen för att extrahera en substräng från ett specifikt index till slutet av en sträng.

```
Kotlin val originalStrang = "Det här är en teststräng"
val nyStrang = originalStrang.substring(8)
```

I detta fall kommer `nyStrang` att innehålla "teststräng", eftersom vi bara anger startindex och inte slutindex.

Du kan också använda `substring()`-funktionen för att få en del av en sträng baserat på ett visst antal tecken. I detta fall behöver du bara ange startindex och längden på den önskade substrängen.

```
Kotlin val originalStrang = "Det här är en teststräng"
val nyStrang = originalStrang.substring(12,3)
```

Detta kommer att returnera en substräng på 3 tecken, vilket i detta fall blir "ett".

## Djupdykning

Det finns flera saker att tänka på när du arbetar med substrängar i Kotlin. En viktig aspekt är att komma ihåg att index i en sträng börjar på 0, vilket betyder att den första bokstaven har index 0, den andra bokstaven har index 1, och så vidare. Detta är viktigt att tänka på när du bestämmer vilka start- och slutindex du behöver för att extrahera önskad substräng.

En annan viktig faktor är att varje gång du använder `substring()`-funktionen, skapar du en helt ny sträng istället för att modifiera den befintliga strängen. Detta kan påverka prestandan om du arbetar med stora textbaserade filer eller programmässigt behöver extrahera många substrängar.

## Se även

- [Kotlin dokumentation för substring-funktionen](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/substring.html)
- [Guide för textbaserade filhanteringsoperationer i Kotlin](https://kotlinlang.org/docs/tutorials/kotlin-for-py/operating-files.html#operating-with-files-and-directories)