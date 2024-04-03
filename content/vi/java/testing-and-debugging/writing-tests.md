---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:09.025684-07:00
description: "L\xE0m nh\u01B0 th\u1EBF n\xE0o: H\xE3y vi\u1EBFt m\u1ED9t b\xE0i ki\u1EC3\
  m th\u1EED \u0111\u01A1n gi\u1EA3n s\u1EED d\u1EE5ng JUnit, m\u1ED9t framework ki\u1EC3\
  m th\u1EED ph\u1ED5 bi\u1EBFn trong Java. Ch\xFAng ta s\u1EBD ki\u1EC3m th\u1EED\
  \ m\u1ED9t ph\u01B0\u01A1ng th\u1EE9c c\u1ED9ng\u2026"
lastmod: '2024-03-13T22:44:36.494137-06:00'
model: gpt-4-0125-preview
summary: "H\xE3y vi\u1EBFt m\u1ED9t b\xE0i ki\u1EC3m th\u1EED \u0111\u01A1n gi\u1EA3\
  n s\u1EED d\u1EE5ng JUnit, m\u1ED9t framework ki\u1EC3m th\u1EED ph\u1ED5 bi\u1EBF\
  n trong Java."
title: "Vi\u1EBFt c\xE1c b\xE0i ki\u1EC3m tra"
weight: 36
---

## Làm như thế nào:
Hãy viết một bài kiểm thử đơn giản sử dụng JUnit, một framework kiểm thử phổ biến trong Java. Chúng ta sẽ kiểm thử một phương thức cộng hai số nguyên.

```java
import static org.junit.jupiter.api.Assertions.assertEquals;
import org.junit.jupiter.api.Test;

public class CalculatorTest {

    @Test
    public void testAddition() {
        Calculator calculator = new Calculator();
        assertEquals(5, calculator.add(2, 3), "2 + 3 phải bằng 5");
    }
}

class Calculator {
    public int add(int a, int b) {
        return a + b;
    }
}
```

Nếu phương thức hoạt động, bài kiểm thử sẽ vượt qua mà không gặp sự cố. Nếu nó thất bại, JUnit in ra lỗi:

```
org.opentest4j.AssertionFailedError: 2 + 3 phải bằng 5 ==> dự kiến: <5> nhưng lại là: <4>
```

## Tìm hiểu sâu hơn
Việc kiểm thử không phải lúc nào cũng là ưu tiên của lập trình viên - nó trở nên phổ biến với sự phát triển Agile và các phương pháp như Test-Driven Development (TDD). Các lựa chọn thay thế cho JUnit bao gồm TestNG và Spock, mỗi cái có những ưu điểm riêng của nó. Việc thực thi các bài kiểm thử tốt là một nghệ thuật; nó thường liên quan đến việc giả lập các phụ thuộc, tuân thủ các mẫu kiểm thử và tích hợp liên tục các bài kiểm thử vào quy trình xây dựng.

## Xem thêm
- JUnit 5 Hướng dẫn người dùng: [https://junit.org/junit5/docs/current/user-guide/](https://junit.org/junit5/docs/current/user-guide/)
- Bài viết về Test-Driven Development: [https://www.agilealliance.org/glossary/tdd/](https://www.agilealliance.org/glossary/tdd/)
- Các framework giả lập: Mockito [https://site.mockito.org/](https://site.mockito.org/)
