---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:51.985743-07:00
description: "Como fazer: Desenvolvedores Java primariamente usam dois frameworks\
  \ de teste: JUnit e TestNG. Aqui, focaremos no JUnit, a escolha mais popular para\u2026"
lastmod: '2024-03-13T22:44:46.459649-06:00'
model: gpt-4-0125-preview
summary: Desenvolvedores Java primariamente usam dois frameworks de teste.
title: Escrevendo testes
weight: 36
---

## Como fazer:
Desenvolvedores Java primariamente usam dois frameworks de teste: JUnit e TestNG. Aqui, focaremos no JUnit, a escolha mais popular para escrever testes devido à sua simplicidade e adoção generalizada.

### Básicos de JUnit
Para usar JUnit no seu projeto Maven, adicione a seguinte dependência ao seu `pom.xml`:

```xml
<dependency>
    <groupId>org.junit.jupiter</groupId>
    <artifactId>junit-jupiter</artifactId>
    <version>5.9.0</version>
    <scope>test</scope>
</dependency>
```

Um teste básico em JUnit se parece com isso:

```java
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class CalculatorTest {
    
    @Test
    public void testAdd() {
        Calculator calculator = new Calculator();
        assertEquals(5, calculator.add(2, 3), "2 + 3 deve ser igual a 5");
    }
}
```

Executar esse teste vai resultar em sucesso, indicando que o método `add` funciona conforme esperado, ou falha, mostrando uma mensagem de erro.

### Simulação com Mockito
Em cenários do mundo real, objetos frequentemente dependem de outros objetos. Mockito é um framework de simulação popular que ajuda na criação de objetos simulados para o propósito de testar.

Adicione Mockito ao seu projeto Maven:

```xml
<dependency>
    <groupId>org.mockito</groupId>
    <artifactId>mockito-core</artifactId>
    <version>4.5.1</version>
    <scope>test</scope>
</dependency>
```

Um caso de uso simples com Mockito:

```java
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.*;

public class UserServiceTest {

    @Test
    public void testGetUsername() {
        // Criar um UserRepository simulado
        UserRepository mockRepository = mock(UserRepository.class);

        // Definir comportamento para o objeto simulado
        when(mockRepository.getUsername(1)).thenReturn("john_doe");

        UserService userService = new UserService(mockRepository);
        
        assertEquals("john_doe", userService.getUsername(1), "ID de Usuário 1 deve ser john_doe");
    }
}
```

Essa simulação nos permite testar `UserService` sem precisar de um `UserRepository` real, focando o teste na lógica dentro do próprio `UserService`.
