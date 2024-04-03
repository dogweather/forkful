---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:19.432525-07:00
description: "L\xE0m th\u1EBF n\xE0o: C\xE1c chu\u1ED7i Java c\xF3 ph\u01B0\u01A1\
  ng th\u1EE9c `length()`. G\u1ECDi n\xF3, v\xE0 b\u1EA1n s\u1EBD nh\u1EADn \u0111\
  \u01B0\u1EE3c s\u1ED1 \u0111\u1EBFm k\xFD t\u1EF1. D\u1EC5 d\xE0ng."
lastmod: '2024-03-13T22:44:36.477301-06:00'
model: gpt-4-0125-preview
summary: "C\xE1c chu\u1ED7i Java c\xF3 ph\u01B0\u01A1ng th\u1EE9c `length()`."
title: "T\xECm chi\u1EC1u d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i k\xFD t\u1EF1"
weight: 7
---

## Làm thế nào:
Các chuỗi Java có phương thức `length()`. Gọi nó, và bạn sẽ nhận được số đếm ký tự. Dễ dàng.

```java
public class StringLengthExample {
    public static void main(String[] args) {
        String greeting = "Hello, World!";
        int length = greeting.length();

        System.out.println("Chiều dài của chuỗi là: " + length);
        // Đầu ra: Chiều dài của chuỗi là: 13
    }
}
```

## Đào sâu
Phương thức `length()` có từ những phiên bản Java đầu tiên, làm cho nó trở thành một phần lâu đời của lớp `String`. Nó đơn giản nhưng thiết yếu. Bên trong, một `String` trong Java được hỗ trợ bởi một mảng ký tự, với phương thức `length()` trả về kích thước của mảng này. Quan trọng là, chuỗi Java là bất biến, vì vậy một khi được tạo, chiều dài không thay đổi, làm cho phương thức này nhanh và đáng tin cậy.

Có phương pháp thay thế không? Chà, ngoài việc tự viết một hàm để đếm ký tự (không cần thiết và không hiệu suất), thực sự không. Hãy nhớ rằng `length()` trả về số lượng đơn vị `char`, không nhất thiết là điểm mã. Đối với các ký tự Unicode không vừa với kích cỡ `char` tiêu chuẩn 16-bit, cân nhắc sử dụng `codePointCount()` nếu bạn cần tính đến các ký tự bổ sung.

## Xem thêm
Đào sâu hơn hoặc khám phá các chủ đề liên quan:
- [Tài liệu về Chuỗi Java](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html)
- [Tài liệu về Lớp Ký Tự Java](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/Character.html) để hiểu thêm về Unicode, ký tự, và điểm mã.
- [Hướng dẫn Java của Oracle](https://docs.oracle.com/javase/tutorial/java/data/strings.html) để hiểu rõ hơn về chuỗi trong Java.
