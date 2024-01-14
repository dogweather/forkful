---
title:    "Java: Å lese kommandolinje-argumenter"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor

De fleste av oss som programmerer i Java har trolig hørt om "kommandolinjeargumenter", men hvorfor er det egentlig viktig å lære å lese dem? Vel, det er faktisk en veldig nyttig ferdighet å ha når du utvikler Java-applikasjoner. Ved å lese kommandolinjeargumenter kan du gi ulike innstillinger og verdier til programmene dine når de kjøres, uten å måtte endre selve koden. Dette gjør prosessen med å teste og kjøre programmet mye enklere og mer fleksibel.

## Hvordan

For å lese kommandolinjeargumenter i Java, må du først forstå noen av de grunnleggende konseptene som er involvert. Kommandolinjeargumenter blir sendt til programmet ditt som en tekststreng, og de kan inneholde både flagg og argumenter. Flagget starter vanligvis med "-", etterfulgt av en bokstav eller et ord som brukes til å identifisere hva slags informasjon som følger. Argumentene kommer vanligvis etter flaggene, og disse er de faktiske verdiene du ønsker at programmet skal lese.

La oss se på et enkelt eksempel på hvordan dette kan implementeres i Java:

```Java
public class CommandLineArgs {
    public static void main(String[] args) {
        // Sjekker om det ble sendt inn noen kommandolinjeargumenter
        if (args.length == 0) {
            System.out.println("Du må oppgi kommandolinjeargumenter!");
        }
        // Hvis det ble sendt inn argumenter, skriver vi de ut
        else {
            System.out.println("Dine kommandolinjeargumenter er: ");
            for (String arg : args) {
                System.out.println(arg);
            }
        }
    }
}
```

I dette eksemplet bruker vi `args`-parameteret til `main`-metoden for å få tilgang til kommandolinjeargumentene som er sendt til programmet. Vi sjekker først om det ble sendt inn noen argumenter, og hvis ikke, gir vi en feilmelding. Hvis det ble sendt inn argumenter, går vi gjennom dem og skriver de ut en etter en.

La oss si at vi kjører programmet med følgende kommandolinjeargumenter:

`java CommandLineArgs -hjelp enkelt argument`

Da vil følgende bli skrevet ut i terminalen:

```Java
Dine kommandolinjeargumenter er:
-hjelp
enkelt
argument
```

Som du ser, blir flagget `-hjelp` og argumentene `enkelt` og `argument` lest og skrevet ut som forventet.

## Dypdykk

I tillegg til å lese enkle kommandolinjeargumenter som i eksemplet ovenfor, kan det også være nødvendig å håndtere mer komplekse situasjoner. Noen ganger trenger vi for eksempel å håndtere argumenter som har mellomrom eller inneholder spesialtegn. I slike tilfeller anbefales det å bruke biblioteker som [Apache Commons CLI](https://commons.apache.org/proper/commons-cli/) for å håndtere dette mer effektivt.

Det er også viktig å merke seg at hvis kommandolinjeargumentene som blir sendt ikke stemmer overens med hva programmet forventer, vil det resultere i en `IllegalArgumentException`. Derfor er det lurt å håndtere dette scenariet på en god måte i koden din.

## Se også

- [Java-kommandolinjeargumenter](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
- [Bruke Commons CLI til å håndtere kommandolinjeargumenter](https://www.baeldung.com/apache-commons-cli)
- [Feilhåndtering i Java](https://www.baeldung.com/java-exceptions)