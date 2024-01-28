---
title:                "Avrunding av tall"
date:                  2024-01-26T03:45:00.207331-07:00
model:                 gpt-4-0125-preview
simple_title:         "Avrunding av tall"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/rounding-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å runde av tall betyr å justere dem til en spesifisert grad av presisjon. Programmerere gjør dette for å forenkle tall for lesbarhet, for å møte visse spesifikasjoner, eller for å sikre at beregninger holder seg innenfor visse grenser, som å unngå presisjonsfeil i flytpunkt-aritmetikk.

## Hvordan:
Java tilbyr flere måter å runde av tall på. Her er en rask demo med `Math.round()`, `BigDecimal`, og `DecimalFormat`.

```java
public class RoundingDemo {
    public static void main(String[] args) {
        double num = 123.4567;

        // Bruker Math.round()
        long roundedNum = Math.round(num);
        System.out.println(roundedNum); // Utdata: 123

        // Bruker BigDecimal for mer kontroll
        BigDecimal bd = new BigDecimal(num).setScale(2, RoundingMode.HALF_UP);
        double roundedBigDecimal = bd.doubleValue();
        System.out.println(roundedBigDecimal); // Utdata: 123.46

        // Bruker DecimalFormat
        DecimalFormat df = new DecimalFormat("#.##");
        String formattedNum = df.format(num);
        System.out.println(formattedNum); // Utdata: 123.46
    }
}
```

## Dypdykk
Historisk har det å runde av tall vært essensielt for analoge beregninger og har blitt overført til digital databehandling for effektivitet og nøyaktighet. Avrundingsfeil, som de fra flytpunkt-aritmetikk, demonstrerer at dette ikke er et trivielt problem – de kan akkumulativt ødelegge beregninger i for eksempel luftfart og finansielle applikasjoner.

Utover `Math.round()`, har du `BigDecimal`, som gir deg finere kontroll over skalaen og avrundingsmodusen, og `DecimalFormat` for når du trenger å runde av tall som en del av formatering av tekstutdata. Alternativer til avrunding inkluderer å avrunde ned, avrunde opp og trunkere, som er forskjellige måter å håndtere presisjon på og vanligvis behandles av ulike `Math`-metoder.

Avhengig av bruksområdet ditt, kan avrundingsstrategien variere. For eksempel er `BigDecimal` veien å gå for finansielle beregninger, hvor presisjon er kritisk. I kontrast er `Math.round()` en rask måte for generelle operasjoner hvor du er mindre kresen på avrundingsmodus.

## Se også
- [Oracles Java Math-dokumentasjon](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/Math.html)
- [IEEE-standarden for flytpunkt-aritmetikk (IEEE 754)](https://ieeexplore.ieee.org/document/4610935)
- [DecimalFormat-klassen i Java](https://docs.oracle.com/javase/7/docs/api/java/text/DecimalFormat.html)
