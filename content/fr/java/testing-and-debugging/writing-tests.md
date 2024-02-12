---
title:                "Rédaction de tests"
aliases: - /fr/java/writing-tests.md
date:                  2024-02-03T19:30:45.157987-07:00
model:                 gpt-4-0125-preview
simple_title:         "Rédaction de tests"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Écrire des tests en Java consiste à vérifier que votre code se comporte comme prévu dans diverses conditions. Les programmeurs écrivent des tests pour prévenir les bugs, garantir que la fonctionnalité reste correcte après des modifications et favoriser les principes de bonne conception logicielle.

## Comment faire :
Les développeurs Java utilisent principalement deux cadres de test : JUnit et TestNG. Ici, nous nous concentrerons sur JUnit, le choix le plus populaire pour écrire des tests en raison de sa simplicité et de son adoption généralisée.

### Bases de JUnit

Pour utiliser JUnit dans votre projet Maven, ajoutez la dépendance suivante à votre `pom.xml` :

```xml
<dependency>
    <groupId>org.junit.jupiter</groupId>
    <artifactId>junit-jupiter</artifactId>
    <version>5.9.0</version>
    <scope>test</scope>
</dependency>
```

Un test de base dans JUnit ressemble à ceci :

```java
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class CalculatorTest {
    
    @Test
    public void testAdd() {
        Calculator calculatrice = new Calculator();
        assertEquals(5, calculatrice.add(2, 3), "2 + 3 devrait être égal à 5");
    }
}
```

L'exécution de ce test se soldera soit par une réussite, indiquant que la méthode `add` fonctionne comme prévu, soit par un échec, affichant un message d'erreur.

### Simulation avec Mockito

Dans des scénarios réels, les objets dépendent souvent d'autres objets. Mockito est un cadre de simulation populaire qui aide à créer des objets mock pour les besoins des tests.

Ajoutez Mockito à votre projet Maven :

```xml
<dependency>
    <groupId>org.mockito</groupId>
    <artifactId>mockito-core</artifactId>
    <version>4.5.1</version>
    <scope>test</scope>
</dependency>
```

Un cas d'utilisation simple avec Mockito :

```java
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.*;

public class UserServiceTest {

    @Test
    public void testGetUsername() {
        // Créez un UserRepository mock
        UserRepository mockRepository = mock(UserRepository.class);

        // Définir le comportement pour l'objet mock
        when(mockRepository.getUsername(1)).thenReturn("john_doe");

        UserService userService = new UserService(mockRepository);
        
        assertEquals("john_doe", userService.getUsername(1), "L'ID utilisateur 1 devrait être john_doe");
    }
}
```

Ce mock nous permet de tester `UserService` sans avoir besoin d'un véritable `UserRepository`, concentrant le test sur la logique au sein de `UserService` lui-même.
