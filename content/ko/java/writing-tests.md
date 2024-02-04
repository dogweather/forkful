---
title:                "테스트 작성하기"
date:                  2024-02-03T19:30:59.673004-07:00
model:                 gpt-4-0125-preview
simple_title:         "테스트 작성하기"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇을, 왜?
자바에서 테스트를 작성하는 것은 다양한 조건에서 코드가 예상대로 동작하는지 확인하는 것에 관한 것입니다. 프로그래머들은 버그를 방지하고, 변경 후에도 기능이 올바르게 유지되도록 보장하며, 좋은 소프트웨어 설계 원칙을 촉진하기 위해 테스트를 작성합니다.

## 방법:
자바 개발자들은 주로 두 가지 테스팅 프레임워크인 JUnit과 TestNG를 사용합니다. 여기서는 간결함과 널리 퍼진 채택으로 인해 테스트 작성에 더 인기 있는 선택인 JUnit에 초점을 맞출 것입니다.

### JUnit 기초

Maven 프로젝트에서 JUnit을 사용하려면, 다음 의존성을 `pom.xml`에 추가하세요:

```xml
<dependency>
    <groupId>org.junit.jupiter</groupId>
    <artifactId>junit-jupiter</artifactId>
    <version>5.9.0</version>
    <scope>test</scope>
</dependency>
```

JUnit에서의 기본적인 테스트는 다음과 같습니다:

```java
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class CalculatorTest {
    
    @Test
    public void testAdd() {
        Calculator calculator = new Calculator();
        assertEquals(5, calculator.add(2, 3), "2 + 3은 5여야 합니다");
    }
}
```

이 테스트를 실행하면 `add` 메서드가 예상대로 작동하여 통과하거나, 오류 메시지를 보여주며 실패합니다.

### Mockito를 사용한 목업

실제 상황에서, 객체는 종종 다른 객체에 의존합니다. Mockito는 테스트를 위한 목업 객체를 생성하는 데 도움이 되는 인기 있는 목업 프레임워크입니다.

Maven 프로젝트에 Mockito를 추가하세요:

```xml
<dependency>
    <groupId>org.mockito</groupId>
    <artifactId>mockito-core</artifactId>
    <version>4.5.1</version>
    <scope>test</scope>
</dependency>
```

Mockito를 사용한 간단한 사례:

```java
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.*;

public class UserServiceTest {

    @Test
    public void testGetUsername() {
        // UserRepository 모킹 생성
        UserRepository mockRepository = mock(UserRepository.class);

        // 목업 객체에 대한 동작 정의
        when(mockRepository.getUsername(1)).thenReturn("john_doe");

        UserService userService = new UserService(mockRepository);
        
        assertEquals("john_doe", userService.getUsername(1), "사용자 ID 1은 john_doe여야 합니다");
    }
}
```

이 목업을 사용하면 `UserRepository` 실제 인스턴스 없이도 `UserService`를 테스트할 수 있어 `UserService` 내부의 로직에 초점을 맞출 수 있습니다.
