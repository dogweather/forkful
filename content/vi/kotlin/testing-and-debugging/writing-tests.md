---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:59.139631-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Kotlin s\u1EED d\u1EE5ng JUnit \u0111\u1EC3\
  \ th\u1EF1c hi\u1EC7n ki\u1EC3m tra. D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch vi\u1EBF\
  t v\xE0 ch\u1EA1y m\u1ED9t b\xE0i ki\u1EC3m tra \u0111\u01A1n gi\u1EA3n."
lastmod: '2024-03-13T22:44:36.606379-06:00'
model: gpt-4-0125-preview
summary: "Kotlin s\u1EED d\u1EE5ng JUnit \u0111\u1EC3 th\u1EF1c hi\u1EC7n ki\u1EC3\
  m tra."
title: "Vi\u1EBFt c\xE1c b\xE0i ki\u1EC3m tra"
weight: 36
---

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
