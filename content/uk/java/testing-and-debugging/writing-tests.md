---
title:                "Письмо тестів"
aliases: - /uk/java/writing-tests.md
date:                  2024-02-03T19:31:10.924488-07:00
model:                 gpt-4-0125-preview
simple_title:         "Письмо тестів"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?
Написання тестів на Java полягає в перевірці того, що ваш код працює так, як очікувалося, в різних умовах. Програмісти пишуть тести, щоб запобігати помилкам, забезпечити правильність функціональності після змін, а також сприяти добрим принципам проектування програмного забезпечення.

## Як це зробити:
Розробники на Java здебільшого використовують два фреймворки для тестування: JUnit та TestNG. Тут ми зосередимось на JUnit, більш популярному виборі для написання тестів через його простоту та широке використання.

### Основи JUnit

Щоб використовувати JUnit у вашому Maven проекті, додайте таку залежність до вашого `pom.xml`:

```xml
<dependency>
    <groupId>org.junit.jupiter</groupId>
    <artifactId>junit-jupiter</artifactId>
    <version>5.9.0</version>
    <scope>test</scope>
</dependency>
```

Базовий тест в JUnit виглядає так:

```java
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class CalculatorTest {
    
    @Test
    public void testAdd() {
        Calculator calculator = new Calculator();
        assertEquals(5, calculator.add(2, 3), "2 + 3 має бути 5");
    }
}
```

Виконання цього тесту або пройде, показуючи, що метод `add` працює, як очікувалося, або не пройде, показуючи повідомлення про помилку.

### Мокування з Mockito

У реальних сценаріях об’єкти часто залежать від інших об’єктів. Mockito – популярний фреймворк для створення мок-об'єктів з метою тестування.

Додайте Mockito до вашого Maven проекту:

```xml
<dependency>
    <groupId>org.mockito</groupId>
    <artifactId>mockito-core</artifactId>
    <version>4.5.1</version>
    <scope>test</scope>
</dependency>
```

Простий випадок використання з Mockito:

```java
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.*;

public class UserServiceTest {

    @Test
    public void testGetUsername() {
        // Створимо мок UserRepository
        UserRepository mockRepository = mock(UserRepository.class);

        // Визначимо поведінку для мок-об'єкту
        when(mockRepository.getUsername(1)).thenReturn("john_doe");

        UserService userService = new UserService(mockRepository);
        
        assertEquals("john_doe", userService.getUsername(1), "ІД користувача 1 має бути john_doe");
    }
}
```

Цей мок дозволяє нам тестувати `UserService` без потреби в реальному `UserRepository`, зосереджуючи тест на логіці всередині `UserService`.
