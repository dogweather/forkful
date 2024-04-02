---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:59.139631-07:00
description: "Vi\u1EBFt test ngh\u0129a l\xE0 l\u1EADp tr\xECnh c\xE1c \u0111o\u1EA1\
  n m\xE3 \u0111\u1EC3 ki\u1EC3m tra xem m\xE3 kh\xE1c ho\u1EA1t \u0111\u1ED9ng \u0111\
  \xFAng hay kh\xF4ng. C\xE1c l\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n vi\u1EC7\
  c n\xE0y \u0111\u1EC3 b\u1EAFt l\u1ED7i s\u1EDBm, ti\u1EBFt ki\u1EC7m\u2026"
lastmod: '2024-03-13T22:44:36.606379-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EBFt test ngh\u0129a l\xE0 l\u1EADp tr\xECnh c\xE1c \u0111o\u1EA1n\
  \ m\xE3 \u0111\u1EC3 ki\u1EC3m tra xem m\xE3 kh\xE1c ho\u1EA1t \u0111\u1ED9ng \u0111\
  \xFAng hay kh\xF4ng. C\xE1c l\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n vi\u1EC7\
  c n\xE0y \u0111\u1EC3 b\u1EAFt l\u1ED7i s\u1EDBm, ti\u1EBFt ki\u1EC7m\u2026"
title: "Vi\u1EBFt c\xE1c b\xE0i ki\u1EC3m tra"
weight: 36
---

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
