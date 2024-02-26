---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:29.425235-07:00
description: "\u0110\u1ECDc c\xE1c \u0111\u1ED1i s\u1ED1 d\xF2ng l\u1EC7nh trong Java\
  \ l\xE0 vi\u1EC7c l\u1EA5y input cung c\u1EA5p b\u1EDFi ng\u01B0\u1EDDi d\xF9ng\
  \ khi h\u1ECD kh\u1EDFi \u0111\u1ED9ng ch\u01B0\u01A1ng tr\xECnh c\u1EE7a b\u1EA1\
  n t\u1EEB console. L\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u\u2026"
lastmod: '2024-02-25T18:49:34.849473-07:00'
model: gpt-4-0125-preview
summary: "\u0110\u1ECDc c\xE1c \u0111\u1ED1i s\u1ED1 d\xF2ng l\u1EC7nh trong Java\
  \ l\xE0 vi\u1EC7c l\u1EA5y input cung c\u1EA5p b\u1EDFi ng\u01B0\u1EDDi d\xF9ng\
  \ khi h\u1ECD kh\u1EDFi \u0111\u1ED9ng ch\u01B0\u01A1ng tr\xECnh c\u1EE7a b\u1EA1\
  n t\u1EEB console. L\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u\u2026"
title: "\u0110\u1ECDc c\xE1c \u0111\u1ED1i s\u1ED1 d\xF2ng l\u1EC7nh"
---

{{< edit_this_page >}}

## Điều gì và Tại sao?

Đọc các đối số dòng lệnh trong Java là việc lấy input cung cấp bởi người dùng khi họ khởi động chương trình của bạn từ console. Lập trình viên làm điều này để khiến ứng dụng của họ phản hồi với nhu cầu của người dùng, linh hoạt xử lý các nhiệm vụ mà không cần giá trị cứng.

## Làm thế nào:

Java thu thập các đối số dòng lệnh mà bạn phát ra với phương thức `main`. Hãy nhìn vào ví dụ ngắn gọn này:

```java
public class CommandLineExample {
    public static void main(String[] args) {
        // Hãy in ra các đối số dòng lệnh
        for(String arg : args) {
            System.out.println(arg);
        }
    }
}
```

Khởi động terminal của bạn, biên dịch với `javac CommandLineExample.java`, và chạy với `java CommandLineExample These Are Command Line Arguments`. Dưới đây là đầu ra của bạn:

```
These
Are
Command
Line
Arguments
```

## Đào sâu

Xuất phát từ C, các đối số dòng lệnh đã là một phần không thể thiếu kể từ thời đại đen tối của lập trình—nghĩ về thẻ đục và chia sẻ thời gian. Java thừa kế công cụ này với lý do chính đáng. Nó cơ bản, linh hoạt và phù hợp với nhiều tình huống.

Có nhiều lựa chọn thay thế? Chắc chắn, có rất nhiều. Các thư viện như JCommander hoặc Apache Commons CLI tăng cường khả năng phân tích của bạn. Họ xử lý các kịch bản phức tạp một cách tinh tế.

Dưới cái nắp, phương thức `main` của Java thu một mảng `String`—`args`. Trong khi máy ảo chạy, khi bạn nhấn `java ClassName`, những gì theo sau là các input của bạn, được lưu trữ gọn gàng trong `args`.

## Xem thêm:

- Để ôn lại các kiến thức cơ bản: [Hướng dẫn Java chính thức của Oracle](https://docs.oracle.com/javase/tutorial/)
- Đào sâu vào JCommander cho việc phân tích phức tạp: [JCommander GitHub](https://github.com/cbeust/jcommander)
- Khám phá Apache Commons CLI: [Apache Commons CLI](https://commons.apache.org/proper/commons-cli/)
