---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:01.561908-07:00
description: "L\xE0m th\u1EBF n\xE0o: Java s\u1EED d\u1EE5ng ngo\u1EA1i l\u1EC7 \u0111\
  \u1EC3 x\u1EED l\xFD l\u1ED7i. B\u1EA1n b\u1ECDc m\xE3 r\u1EE7i ro trong m\u1ED9\
  t kh\u1ED1i `try` v\xE0 b\u1EAFt ngo\u1EA1i l\u1EC7 b\u1EB1ng `catch`. D\u01B0\u1EDB\
  i \u0111\xE2y l\xE0 m\u1ED9t v\xED d\u1EE5 \u0111\u01A1n gi\u1EA3n."
lastmod: '2024-03-13T22:44:36.499180-06:00'
model: gpt-4-0125-preview
summary: "Java s\u1EED d\u1EE5ng ngo\u1EA1i l\u1EC7 \u0111\u1EC3 x\u1EED l\xFD l\u1ED7\
  i."
title: "X\u1EED l\xFD l\u1ED7i"
weight: 16
---

## Làm thế nào:
Java sử dụng ngoại lệ để xử lý lỗi. Bạn bọc mã rủi ro trong một khối `try` và bắt ngoại lệ bằng `catch`. Dưới đây là một ví dụ đơn giản:

```java
public class ErrorHandlingExample {
    public static void main(String[] args) {
        try {
            int result = divide(10, 0);
            System.out.println("Kết quả là: " + result);
        } catch (ArithmeticException e) {
            System.out.println("Rất tiếc, không thể chia cho không!");
        }
    }

    private static int divide(int tử số, int mẫu số) {
        return tử số / mẫu số;
    }
}
```

Kết quả:
```
Rất tiếc, không thể chia cho không!
```

## Đào sâu
Xử lý lỗi trong Java đã phát triển. Những ngày đầu không có ngoại lệ; lập trình viên kiểm tra mã lỗi. Sau đó, Java giới thiệu các khối try-catch, cho phép xử lý lỗi một cách tinh tế hơn.

Các phương pháp thay thế cho `try-catch` truyền thống bao gồm `try-with-resources` cho việc tự động đóng tài nguyên và mã sạch hơn, được giới thiệu trong Java 7.

Chi tiết thực hiện có ý nghĩa. Ví dụ, bắt `Exception` hay `Throwable` thường là thực hành xấu. Đó là quá rộng, che giấu các lỗi mà bạn có thể không nhận thức được. Hãy giữ ở các ngoại lệ cụ thể.

## Tham khảo thêm
- Hướng dẫn chính thức về ngoại lệ của Oracle Java: [https://docs.oracle.com/javase/tutorial/essential/exceptions/](https://docs.oracle.com/javase/tutorial/essential/exceptions/)
- Tài liệu về câu lệnh `try-with-resources` của Java: [https://docs.oracle.com/javase/tutorial/essential/exceptions/tryResourceClose.html](https://docs.oracle.com/javase/tutorial/essential/exceptions/tryResourceClose.html)
- Effective Java của Joshua Bloch, để tham khảo các phương pháp tốt nhất về ngoại lệ.
