---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:29.425235-07:00
description: "L\xE0m th\u1EBF n\xE0o: Java thu th\u1EADp c\xE1c \u0111\u1ED1i s\u1ED1\
  \ d\xF2ng l\u1EC7nh m\xE0 b\u1EA1n ph\xE1t ra v\u1EDBi ph\u01B0\u01A1ng th\u1EE9\
  c `main`. H\xE3y nh\xECn v\xE0o v\xED d\u1EE5 ng\u1EAFn g\u1ECDn n\xE0y."
lastmod: '2024-03-13T22:44:36.509441-06:00'
model: gpt-4-0125-preview
summary: "Java thu th\u1EADp c\xE1c \u0111\u1ED1i s\u1ED1 d\xF2ng l\u1EC7nh m\xE0\
  \ b\u1EA1n ph\xE1t ra v\u1EDBi ph\u01B0\u01A1ng th\u1EE9c `main`."
title: "\u0110\u1ECDc c\xE1c \u0111\u1ED1i s\u1ED1 d\xF2ng l\u1EC7nh"
weight: 23
---

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
