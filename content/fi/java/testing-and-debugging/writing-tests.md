---
title:                "Testien kirjoittaminen"
date:                  2024-02-03T19:31:27.678766-07:00
model:                 gpt-4-0125-preview
simple_title:         "Testien kirjoittaminen"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mitä & Miksi?
Testien kirjoittaminen Javassa koskee koodisi toiminnan varmistamista odotetulla tavalla eri olosuhteissa. Ohjelmoijat kirjoittavat testejä vikojen estämiseksi, varmistaakseen toiminnallisuuden pysyvän oikeana muutosten jälkeen ja edistääkseen hyviä ohjelmistosuunnittelun periaatteita.

## Kuinka:
Java-kehittäjät käyttävät pääasiassa kahta testauskehystä: JUnitia ja TestNG:tä. Tässä keskitymme JUnitiin, joka on suositumpi valinta testien kirjoittamiseen sen yksinkertaisuuden ja laajan käyttöönoton vuoksi.

### JUnit Perusteet

JUnitin käyttöönotto Maven-projektissasi vaatii seuraavan riippuvuuden lisäämistä `pom.xml`-tiedostoosi:

```xml
<dependency>
    <groupId>org.junit.jupiter</groupId>
    <artifactId>junit-jupiter</artifactId>
    <version>5.9.0</version>
    <scope>test</scope>
</dependency>
```

Perustestaus JUnitissa näyttää tältä:

```java
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class CalculatorTest {
    
    @Test
    public void testAdd() {
        Calculator calculator = new Calculator();
        assertEquals(5, calculator.add(2, 3), "2 + 3 pitäisi olla 5");
    }
}
```

Tämän testin suorittaminen joko läpäisee, mikä osoittaa, että `add`-metodi toimii odotetusti, tai epäonnistuu, näyttäen virheviestin.

### Mockaus Mockitoa käyttäen

Todellisissa tilanteissa, oliot usein riippuvat muista olioista. Mockito on suosittu mockauskehys, joka auttaa luomaan mock-olioita testaustarkoituksessa.

Lisää Mockito Maven-projektiisi:

```xml
<dependency>
    <groupId>org.mockito</groupId>
    <artifactId>mockito-core</artifactId>
    <version>4.5.1</version>
    <scope>test</scope>
</dependency>
```

Yksinkertainen käyttötapaus Mockitoa käyttäen:

```java
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.*;

public class UserServiceTest {

    @Test
    public void testGetUsername() {
        // Luo mock UserRepository
        UserRepository mockRepository = mock(UserRepository.class);

        // Määrittele käyttäytymistä mock-oliolle
        when(mockRepository.getUsername(1)).thenReturn("john_doe");

        UserService userService = new UserService(mockRepository);
        
        assertEquals("john_doe", userService.getUsername(1), "Käyttäjä-ID 1 pitäisi olla john_doe");
    }
}
```

Tämä mock mahdollistaa `UserService`-testaamisen ilman oikeaa `UserRepository`-instanssia, keskittyen testissä `UserService`-logiikan testaamiseen.
