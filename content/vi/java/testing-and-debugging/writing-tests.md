---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:09.025684-07:00
description: "Vi\u1EBFt ki\u1EC3m th\u1EED l\xE0 t\u1EA1o ra m\xE3 l\u1EC7nh ki\u1EC3\
  m tra xem m\xE3 l\u1EC7nh kh\xE1c c\xF3 ho\u1EA1t \u0111\u1ED9ng ch\xEDnh x\xE1\
  c hay kh\xF4ng. L\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3\
  \ b\u1EAFt l\u1ED7i s\u1EDBm, \u0111\u1EA3m b\u1EA3o ph\u1EA7n m\u1EC1m\u2026"
lastmod: 2024-02-19 22:04:55.657335
model: gpt-4-0125-preview
summary: "Vi\u1EBFt ki\u1EC3m th\u1EED l\xE0 t\u1EA1o ra m\xE3 l\u1EC7nh ki\u1EC3\
  m tra xem m\xE3 l\u1EC7nh kh\xE1c c\xF3 ho\u1EA1t \u0111\u1ED9ng ch\xEDnh x\xE1\
  c hay kh\xF4ng. L\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3\
  \ b\u1EAFt l\u1ED7i s\u1EDBm, \u0111\u1EA3m b\u1EA3o ph\u1EA7n m\u1EC1m\u2026"
title: "Vi\u1EBFt c\xE1c b\xE0i ki\u1EC3m tra"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Viết kiểm thử là tạo ra mã lệnh kiểm tra xem mã lệnh khác có hoạt động chính xác hay không. Lập trình viên làm điều này để bắt lỗi sớm, đảm bảo phần mềm hoạt động như mong đợi, và duy trì chất lượng mã theo thời gian.

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
