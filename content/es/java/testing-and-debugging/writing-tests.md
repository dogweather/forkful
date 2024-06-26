---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:44.518849-07:00
description: "C\xF3mo hacerlo: Los desarrolladores de Java usan principalmente dos\
  \ marcos de pruebas: JUnit y TestNG. Aqu\xED, nos centraremos en JUnit, la elecci\xF3\
  n m\xE1s\u2026"
lastmod: '2024-03-13T22:44:58.941054-06:00'
model: gpt-4-0125-preview
summary: Los desarrolladores de Java usan principalmente dos marcos de pruebas.
title: Escribiendo pruebas
weight: 36
---

## Cómo hacerlo:
Los desarrolladores de Java usan principalmente dos marcos de pruebas: JUnit y TestNG. Aquí, nos centraremos en JUnit, la elección más popular para escribir pruebas debido a su simplicidad y adopción generalizada.

### Fundamentos de JUnit
Para usar JUnit en tu proyecto Maven, añade la siguiente dependencia a tu `pom.xml`:

```xml
<dependency>
    <groupId>org.junit.jupiter</groupId>
    <artifactId>junit-jupiter</artifactId>
    <version>5.9.0</version>
    <scope>test</scope>
</dependency>
```

Una prueba básica en JUnit luce así:

```java
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class CalculatorTest {

    @Test
    public void testAdd() {
        Calculator calculator = new Calculator();
        assertEquals(5, calculator.add(2, 3), "2 + 3 debería ser igual a 5");
    }
}
```

Ejecutar esta prueba resultará en un éxito, indicando que el método `add` funciona como se espera, o en un fallo, mostrando un mensaje de error.

### Simulación con Mockito
En escenarios del mundo real, los objetos a menudo dependen de otros objetos. Mockito es un marco de simulación popular que ayuda en la creación de objetos simulados con el propósito de probar.

Agrega Mockito a tu proyecto Maven:

```xml
<dependency>
    <groupId>org.mockito</groupId>
    <artifactId>mockito-core</artifactId>
    <version>4.5.1</version>
    <scope>test</scope>
</dependency>
```

Un caso de uso sencillo con Mockito:

```java
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.*;

public class UserServiceTest {

    @Test
    public void testGetUsername() {
        // Crear un UserRepository simulado
        UserRepository mockRepository = mock(UserRepository.class);

        // Definir comportamiento para el objeto simulado
        when(mockRepository.getUsername(1)).thenReturn("john_doe");

        UserService userService = new UserService(mockRepository);
        
        assertEquals("john_doe", userService.getUsername(1), "El ID de usuario 1 debería ser john_doe");
    }
}
```

Esta simulación nos permite probar `UserService` sin necesitar un `UserRepository` real, enfocando la prueba en la lógica dentro de `UserService` en sí.
