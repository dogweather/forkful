---
title:                "Viết các bài kiểm tra"
aliases:
- /vi/kotlin/writing-tests.md
date:                  2024-01-28T22:12:59.139631-07:00
model:                 gpt-4-0125-preview
simple_title:         "Viết các bài kiểm tra"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/kotlin/writing-tests.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Viết test nghĩa là lập trình các đoạn mã để kiểm tra xem mã khác hoạt động đúng hay không. Các lập trình viên thực hiện việc này để bắt lỗi sớm, tiết kiệm thời gian và đảm bảo phần mềm thực hiện đúng những gì nó cần làm một cách nhất quán.

## Cách thực hiện:
Kotlin sử dụng JUnit để thực hiện kiểm tra. Dưới đây là cách viết và chạy một bài kiểm tra đơn giản:

```kotlin
import org.junit.Assert.assertEquals
import org.junit.Test

class CalculatorTest {
    
    @Test
    fun `adds two numbers`() {
        assertEquals(4, Calculator.add(2, 2))
    }
}

object Calculator {
    fun add(a: Int, b: Int) = a + b
}
```

Chạy nó. Nếu kết quả đầu ra của bạn như thế này, bạn đã thành công:

```
Test passed
```

## Tìm hiểu sâu hơn
JUnit, bộ khung kiểm tra chính cho Kotlin, có nguồn gốc từ Java. Các bộ khung kiểm tra thay thế bao gồm Spek và Kotest, mỗi cái có cú pháp và tính năng riêng biệt. Viết test thường liên quan đến việc hiểu cấu trúc của SUT (System Under Test - Hệ thống đang được kiểm tra), mô phỏng các phụ thuộc với MockK hoặc tương tự, và biết sự khác biệt giữa kiểm tra đơn vị, kiểm tra tích hợp và kiểm tra chức năng.

## Xem thêm
- Hướng dẫn sử dụng JUnit 5: [junit.org/junit5/docs/current/user-guide/](https://junit.org/junit5/docs/current/user-guide/)
- Thư viện MockK: [mockk.io](https://mockk.io)
- Bộ khung Spek: [spekframework.org](https://spekframework.org)
- Kotest: [kotest.io](https://kotest.io)
