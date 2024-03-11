---
date: 2024-01-26 01:18:50.944324-07:00
description: "Omstrukturering er prosessen med \xE5 restrukturere eksisterende dataprogramkode\u2014\
  endre faktoriseringen\u2014uten \xE5 endre dens eksterne oppf\xF8rsel. Programmerere\u2026"
lastmod: '2024-03-11T00:14:14.216907-06:00'
model: gpt-4-0125-preview
summary: "Omstrukturering er prosessen med \xE5 restrukturere eksisterende dataprogramkode\u2014\
  endre faktoriseringen\u2014uten \xE5 endre dens eksterne oppf\xF8rsel. Programmerere\u2026"
title: Refaktorering
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Omstrukturering er prosessen med å restrukturere eksisterende dataprogramkode—endre faktoriseringen—uten å endre dens eksterne oppførsel. Programmerere gjør dette for å forbedre de ikke-funksjonelle attributtene til programvaren, forbedre lesbarheten, redusere kompleksiteten og gjøre koden mer vedlikeholdbar for fremtidige prosjekter.

## Hvordan:
La oss ta en enkel Java-klasse som skriker etter omstrukturering på grunn av dårlig organisering og mangel på klarhet.

```java
public class Calculator {
    public int calc(int op1, int op2, String operation) {
        if (operation.equals("add")) {
            return op1 + op2;
        } else if (operation.equals("subtract")) {
            return op1 - op2;
        } // Andre operasjoner...
    }
}
```

Etter omstrukturering har vi:

```java
public class Calculator {
    public int add(int operand1, int operand2) {
        return operand1 + operand2;
    }

    public int subtract(int operand1, int operand2) {
        return operand1 - operand2;
    }

    // Andre operasjoner...
}
```

Ved omstrukturering har vi forbedret metodenes navn og parameterne for lesbarhet og fjernet behovet for en betinget forgreining innenfor en enkelt metode. Hver operasjon angir nå tydelig sitt formål.

## Dypdykk:
Omstrukturering har sine røtter i Smalltalk-samfunnet, med sin vektlegging på kodelesbarhet og objektorientert design, men det tok virkelig av i Java-verdenen på slutten av 90-tallet og tidlig på 00-tallet, spesielt etter publiseringen av Martin Fowlers banebrytende bok, "Refactoring: Improving the Design of Existing Code."

Det finnes alternativer til omstrukturering, som å skrive om koden fra bunnen av. Men omstrukturering foretrekkes ofte fordi det innebærer inkrementelle endringer som ikke forstyrrer funksjonaliteten til applikasjonen.

Implementeringsdetaljer når det gjelder omstrukturering i Java (eller ethvert programmeringsspråk) dreier seg om å forstå kode lukt—indikatorer på dypere problemer i koden. Noen lukter inkluderer lange metoder, store klasser, duplisert kode og overdreven bruk av primitive typer. Ved å anvende omstruktureringsmønstre som Ekstraher Metode, Flytt Metode eller Erstatt Temp med Forespørsel, kan utviklere systematisk adressere disse luktene samtidig som de sikrer at koden forblir funksjonell til enhver tid.

Automatiserte verktøy, som IntelliJ IDEAs støtte for omstrukturering, eller utvidelser for Eclipse, kan bistå prosessen ved å automatisere omstruktureringer som å omdøpe variabler, metoder og klasser, ekstrahere metoder eller variabler, og flytte metoder eller klasser til forskjellige pakker eller navneområder.

## Se Også:
- Martin Fowlers "Refactoring: Improving the Design of Existing Code": https://martinfowler.com/books/refactoring.html
- Omstruktureringsteknikker på Refactoring.Guru: https://refactoring.guru/refactoring/techniques
- Automatisert omstrukturering i Eclipse: https://www.eclipse.org/eclipse/news/4.18/jdt.php
- IntelliJ IDEAs omstruktureringsfunksjoner: https://www.jetbrains.com/idea/features/refactoring.html

Hver av disse ressursene gir enten et grunnlag for å forstå prinsippene for omstrukturering eller verktøy som kan utnyttes for å sette disse prinsippene ut i praksis.
