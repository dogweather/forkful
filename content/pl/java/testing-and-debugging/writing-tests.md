---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:00.434714-07:00
description: "Pisanie test\xF3w w Java ma na celu weryfikacj\u0119, czy Tw\xF3j kod\
  \ zachowuje si\u0119 zgodnie z oczekiwaniami w r\xF3\u017Cnych warunkach. Programi\u015B\
  ci pisz\u0105 testy, aby\u2026"
lastmod: 2024-02-19 22:04:54.410347
model: gpt-4-0125-preview
summary: "Pisanie test\xF3w w Java ma na celu weryfikacj\u0119, czy Tw\xF3j kod zachowuje\
  \ si\u0119 zgodnie z oczekiwaniami w r\xF3\u017Cnych warunkach. Programi\u015Bci\
  \ pisz\u0105 testy, aby\u2026"
title: "Pisanie test\xF3w"
---

{{< edit_this_page >}}

## Co i dlaczego?
Pisanie testów w Java ma na celu weryfikację, czy Twój kod zachowuje się zgodnie z oczekiwaniami w różnych warunkach. Programiści piszą testy, aby zapobiegać błędom, zapewnić poprawność funkcjonalności po zmianach i promować dobre zasady projektowania oprogramowania.

## Jak to robić:
Programiści Java głównie używają dwóch frameworków testowych: JUnit i TestNG. Tutaj skupimy się na JUnit, który jest popularniejszym wyborem do pisania testów ze względu na swoją prostotę i szerokie przyjęcie.

### Podstawy JUnit

Aby użyć JUnit w projekcie Maven, dodaj następującą zależność do pliku `pom.xml`:

```xml
<dependency>
    <groupId>org.junit.jupiter</groupId>
    <artifactId>junit-jupiter</artifactId>
    <version>5.9.0</version>
    <scope>test</scope>
</dependency>
```

Podstawowy test w JUnit wygląda tak:

```java
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class CalculatorTest {
    
    @Test
    public void testAdd() {
        Calculator calculator = new Calculator();
        assertEquals(5, calculator.add(2, 3), "2 + 3 powinno równać się 5");
    }
}
```

Wykonanie tego testu zakończy się sukcesem, co oznacza, że metoda `add` działa zgodnie z oczekiwaniami, lub porażką, wyświetlając komunikat o błędzie.

### Mokowanie z użyciem Mockito

W rzeczywistych scenariuszach, obiekty często zależą od innych obiektów. Mockito to popularny framework do mokowania, który pomaga w tworzeniu mokowanych obiektów do celów testowych.

Dodaj Mockito do swojego projektu Maven:

```xml
<dependency>
    <groupId>org.mockito</groupId>
    <artifactId>mockito-core</artifactId>
    <version>4.5.1</version>
    <scope>test</scope>
</dependency>
```

Prosty przypadek użycia z Mockito:

```java
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.*;

public class UserServiceTest {

    @Test
    public void testGetUsername() {
        // Utworzenie mokowanego UserRepository
        UserRepository mockRepository = mock(UserRepository.class);

        // Zdefiniowanie zachowania dla mokowanego obiektu
        when(mockRepository.getUsername(1)).thenReturn("john_doe");

        UserService userService = new UserService(mockRepository);
        
        assertEquals("john_doe", userService.getUsername(1), "User ID 1 powinien być john_doe");
    }
}
```

Ten mok pozwala nam testować `UserService` bez potrzeby posiadania rzeczywistego `UserRepository`, skupiając test na logice znajdującej się w samym `UserService`.
