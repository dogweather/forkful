---
title:                "Java: Skriving til standardfeil"
simple_title:         "Skriving til standardfeil"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Hvorfor

Å skrive til standard error i Java er en viktig del av feilhåndtering og debugging i applikasjoner. Dette lar deg få tilbakemelding og viktig informasjon om eventuelle problemer eller feil som oppstår under kjøring av programmet ditt. Det er også viktig for å sikre at brukeren får en god opplevelse når de bruker applikasjonen din.

# Hvordan du gjør det

For å skrive til standard error i Java, bruker du System.err.println() -funksjonen. Dette vil skrive ut meldingen til standard error-strømmen i stedet for standard ut-strømmen. Det er viktig å merke seg at du må inkludere "System" -biblioteket for å bruke denne funksjonen i koden din.

```Java
System.err.println("Dette er en feilmelding.");
```

Når du kjører dette eksempelet, vil du få følgende utskrift:

```Java
Dette er en feilmelding.
```

I tillegg til å bruke System.err.println (), kan du også bruke System.err.print () -funksjonen for å skrive ut en melding uten en ny linje. Dette kan være nyttig når du vil skrive ut flere linjer til standard error-strømmen.

# Dypdykk

Standard error-strømmen er en viktig del av Java-programmering for feilhåndtering. Det er spesielt nyttig når du utvikler større applikasjoner som krever feillogging og feilsøking. Ved å skrive til standard error kan du få detaljert informasjon om hvor og når feil oppstår, slik at du raskt kan identifisere og løse problemer.

Det er også viktig å merke seg at standard error-strømmen ikke påvirkes av eventuelle omdirigeringsoperasjoner i koden din. Dette betyr at du alltid vil se feilmeldinger utskrevet direkte til konsollen, selv om du har endret standard ut-strømmen til å skrive til en fil eller en annen destinasjon.

# Se også

- [Hvordan utvikle Java-programmer for feillogging og feilsøking](link til ressurs)
- [Feilhåndtering og unntakshåndtering i Java](link til ressurs)
- [Java System-biblioteket](link til ressurs)