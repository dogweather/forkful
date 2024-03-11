---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:44.518849-07:00
description: "Escribir pruebas en Java se trata de verificar que tu c\xF3digo se comporta\
  \ como se espera bajo varias condiciones. Los programadores escriben pruebas para\u2026"
lastmod: '2024-03-11T00:14:32.756217-06:00'
model: gpt-4-0125-preview
summary: "Escribir pruebas en Java se trata de verificar que tu c\xF3digo se comporta\
  \ como se espera bajo varias condiciones. Los programadores escriben pruebas para\u2026"
title: Escribiendo pruebas
---

{{< edit_this_page >}}

## Qué y Por Qué?
Escribir pruebas en Java se trata de verificar que tu código se comporta como se espera bajo varias condiciones. Los programadores escriben pruebas para prevenir errores, asegurar que la funcionalidad permanezca correcta después de cambios, y fomentar buenos principios de diseño de software.

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
