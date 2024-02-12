---
title:                "Viết các bài kiểm tra"
aliases:
- vi/java/writing-tests.md
date:                  2024-01-28T22:13:09.025684-07:00
model:                 gpt-4-0125-preview
simple_title:         "Viết các bài kiểm tra"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/java/writing-tests.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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
