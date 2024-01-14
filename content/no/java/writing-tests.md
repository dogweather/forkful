---
title:    "Java: Skrive tester"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/java/writing-tests.md"
---

{{< edit_this_page >}}

# Hvorfor

Å skrive tester er en viktig del av utviklingsprosessen i Java-programmering. Det hjelper deg med å identifisere og forebygge feil i koden din, og sikrer at programmet fungerer som forventet. Å skrive gode tester kan også spare mye tid og stress på sikt.

# Hvordan gjøre det

For å skrive tester i Java, må du først lage en egen testklasse som inneholder dine testmetoder. La oss se på et eksempel:

```java
public class MinKlasseTest {

    @Test
    public void testFunksjon1() {
        // Sett opp data
        // Kjør funksjonen som skal testes
        // Sammenlign forventet resultat med faktisk resultat
        // Assert metoder for å verifisere resultatene
    }

    @Test
    public void testFunksjon2() {
        // Sett opp data
        // Kjør funksjonen som skal testes
        // Sammenlign forventet resultat med faktisk resultat
        // Assert metoder for å verifisere resultatene
    }
}
```

Her har vi opprettet en testklasse, kalt "MinKlasseTest", og to testmetoder "testFunksjon1" og "testFunksjon2". Inne i hver testmetode setter vi opp data, kjører funksjonen som skal testes og sammenligner resultatene ved hjelp av assert metoder. Dette lar oss verifisere om funksjonen gir riktig resultat for forskjellige tilfeller.

Det er også viktig å huske å bruke annotasjonen "@Test" før hver testmetode for at den skal bli kjørt som en test. Du kan også bruke andre annotasjoner som "@Before" og "@After" for å sette opp og rydde opp etter testene.

# Dypdykk

Å skrive gode tester innebærer å dekke alle deler av koden din og sikre at de fungerer som forventet. Det er viktig å skrive tester som er uavhengige av hverandre, slik at et feilfunksjonspunkt ikke påvirker resten av testene.

En annen viktig ting å huske på er å ha tilstrekkelig testdekning for koden din. Dette betyr å sørge for at alle grenseverdier og potensielle feil blir testet. Å skrive tester for hjelpefunksjoner og grensesnitt er også viktig for å få en grundig testdekning.

Det finnes også flere verktøy og rammeverk som kan hjelpe deg med å skrive, kjøre og rapportere tester i Java. Noen av de mest populære er JUnit, TestNG og Mockito. Utforsk disse og finn ut hvilket som passer best for dine behov.

# Se også

- [Java Testing Tutorial på baeldung.com](https://www.baeldung.com/java-testing)
- [JUnit User Guide](https://junit.org/junit5/docs/current/user-guide/)
- [TestNG Beginner's Guide](https://testng.org/doc/documentation-main.html)