---
title:                "Skrive tester"
aliases:
- /no/java/writing-tests/
date:                  2024-02-03T19:31:06.303098-07:00
model:                 gpt-4-0125-preview
simple_title:         "Skrive tester"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å skrive tester i Java handler om å verifisere at koden din oppfører seg som forventet under forskjellige forhold. Programmerere skriver tester for å forebygge feil, sikre at funksjonaliteten forblir korrekt etter endringer, og fremme gode prinsipper for programvaredesign.

## Hvordan:
Java-utviklere bruker primært to rammeverk for testing: JUnit og TestNG. Her vil vi fokusere på JUnit, det mer populære valget for å skrive tester på grunn av dets enkelhet og utbredte adopsjon.

### Grunnleggende om JUnit

For å bruke JUnit i ditt Maven-prosjekt, legg til følgende avhengighet i din `pom.xml`:

```xml
<dependency>
    <groupId>org.junit.jupiter</groupId>
    <artifactId>junit-jupiter</artifactId>
    <version>5.9.0</version>
    <scope>test</scope>
</dependency>
```

En grunnleggende test i JUnit ser slik ut:

```java
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class CalculatorTest {
    
    @Test
    public void testAdd() {
        Calculator kalkulator = new Calculator();
        assertEquals(5, kalkulator.add(2, 3), "2 + 3 skal være lik 5");
    }
}
```

Å utføre denne testen vil enten passere, noe som indikerer at `add`-metoden fungerer som forventet, eller feile, og vise en feilmelding.

### Mocking med Mockito

I virkelige scenarioer er objekter ofte avhengige av andre objekter. Mockito er et populært mocking-rammeverk som hjelper i å opprette mock-objekter for formålet med testing.

Legg til Mockito i ditt Maven-prosjekt:

```xml
<dependency>
    <groupId>org.mockito</groupId>
    <artifactId>mockito-core</artifactId>
    <version>4.5.1</version>
    <scope>test</scope>
</dependency>
```

Et enkelt brukstilfelle med Mockito:

```java
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.*;

public class UserServiceTest {

    @Test
    public void testGetUsername() {
        // Opprett en mock UserRepository
        UserRepository mockRepository = mock(UserRepository.class);

        // Definer oppførsel for mock-objektet
        when(mockRepository.getUsername(1)).thenReturn("john_doe");

        UserService userService = new UserService(mockRepository);
        
        assertEquals("john_doe", userService.getUsername(1), "Bruker-ID 1 skal være john_doe");
    }
}
```

Denne mocken lar oss teste `UserService` uten å trenge et faktisk `UserRepository`, og fokuserer testen på logikken innenfor `UserService` selv.
