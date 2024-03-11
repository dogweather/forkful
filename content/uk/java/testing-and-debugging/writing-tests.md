---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:10.924488-07:00
description: "\u041D\u0430\u043F\u0438\u0441\u0430\u043D\u043D\u044F \u0442\u0435\u0441\
  \u0442\u0456\u0432 \u043D\u0430 Java \u043F\u043E\u043B\u044F\u0433\u0430\u0454\
  \ \u0432 \u043F\u0435\u0440\u0435\u0432\u0456\u0440\u0446\u0456 \u0442\u043E\u0433\
  \u043E, \u0449\u043E \u0432\u0430\u0448 \u043A\u043E\u0434 \u043F\u0440\u0430\u0446\
  \u044E\u0454 \u0442\u0430\u043A, \u044F\u043A \u043E\u0447\u0456\u043A\u0443\u0432\
  \u0430\u043B\u043E\u0441\u044F, \u0432 \u0440\u0456\u0437\u043D\u0438\u0445 \u0443\
  \u043C\u043E\u0432\u0430\u0445. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\
  \u0441\u0442\u0438 \u043F\u0438\u0448\u0443\u0442\u044C \u0442\u0435\u0441\u0442\
  \u0438, \u0449\u043E\u0431 \u0437\u0430\u043F\u043E\u0431\u0456\u0433\u0430\u0442\
  \u0438\u2026"
lastmod: '2024-03-11T00:14:22.934074-06:00'
model: gpt-4-0125-preview
summary: "\u041D\u0430\u043F\u0438\u0441\u0430\u043D\u043D\u044F \u0442\u0435\u0441\
  \u0442\u0456\u0432 \u043D\u0430 Java \u043F\u043E\u043B\u044F\u0433\u0430\u0454\
  \ \u0432 \u043F\u0435\u0440\u0435\u0432\u0456\u0440\u0446\u0456 \u0442\u043E\u0433\
  \u043E, \u0449\u043E \u0432\u0430\u0448 \u043A\u043E\u0434 \u043F\u0440\u0430\u0446\
  \u044E\u0454 \u0442\u0430\u043A, \u044F\u043A \u043E\u0447\u0456\u043A\u0443\u0432\
  \u0430\u043B\u043E\u0441\u044F, \u0432 \u0440\u0456\u0437\u043D\u0438\u0445 \u0443\
  \u043C\u043E\u0432\u0430\u0445. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\
  \u0441\u0442\u0438 \u043F\u0438\u0448\u0443\u0442\u044C \u0442\u0435\u0441\u0442\
  \u0438, \u0449\u043E\u0431 \u0437\u0430\u043F\u043E\u0431\u0456\u0433\u0430\u0442\
  \u0438\u2026"
title: "\u041F\u0438\u0441\u044C\u043C\u043E \u0442\u0435\u0441\u0442\u0456\u0432"
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
