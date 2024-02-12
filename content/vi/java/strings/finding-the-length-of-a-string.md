---
title:                "Tìm chiều dài của một chuỗi ký tự"
date:                  2024-01-28T22:00:19.432525-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tìm chiều dài của một chuỗi ký tự"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/java/finding-the-length-of-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Tìm chiều dài của một chuỗi nghĩa là xác định số ký tự mà nó chứa. Lập trình viên thường làm điều này để xác thực đầu vào, lặp qua các ký tự, hoặc căn chỉnh văn bản.

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
