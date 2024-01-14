---
title:    "Java: Utskrift av feilsøkingsutdata"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/java/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive ut feilsøkingsmeldinger er en viktig del av å utvikle Java-programmer. Det lar deg utforske din kode, finne eventuelle feil og løse dem for å sikre at programmet kjører jevnt. Det er også en god måte å få et bedre innsyn i hvordan koden din fungerer.

## Slik gjør du det

For å skrive ut feilsøkingsmeldinger i Java, bruker du metoden `System.out.println()`. Denne metoden skriver ut en melding til konsollet, som kan være nyttig når du prøver å forstå programflyten eller finne ut hvor i koden en feil oppstår.

La oss se et enkelt eksempel på hvordan du kan bruke `System.out.println()`:

```Java
public class Eksempel {

    public static void main(String[] args) {
        int tall = 10;
        System.out.println("Verdien av variabelen tall er: " + tall);
    }
}
```

I dette eksempelet oppretter vi en variabel `tall` med verdien 10. Deretter bruker vi `System.out.println()` til å skrive ut en melding som inneholder verdien til variabelen. Når programmet kjører, vil konsollet vise følgende output: "Verdien av variabelen tall er: 10". Dette viser oss at variabelen `tall` faktisk har verdien 10, og vi kan bruke denne informasjonen til å bedre forstå programmet vårt.

## Dypdykk

Det er flere måter å skrive ut feilsøkingsmeldinger på i Java, avhengig av hva du ønsker å oppnå. `System.out.println()`-metoden er den enkleste, men du kan også bruke `System.out.print()` eller `System.out.printf()` for å formatere utskriften på ulike måter.

En annen nyttig måte å skrive ut feilsøkingsmeldinger på, er ved hjelp av loggelementer. Ved å bruke et loggelement, kan du definere forskjellige nivåer av meldinger basert på alvoret til feilen. Dette gjør det enklere å finne og løse feil i koden din.

## Se også

- [Java API for System.out](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html#out)
- [Java Logging with SLF4J](https://www.baeldung.com/slf4j-logging)
- [Debugging in Java](https://www.baeldung.com/java-debugging)