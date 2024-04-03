---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:23.740317-07:00
description: "L\xE0m th\u1EBF n\xE0o: H\xE3y \u0111\u1EC3 m\u1ED9t \xEDt m\xE3 xu\u1EA5\
  t hi\u1EC7n tr\xEAn m\xE0n h\xECnh."
lastmod: '2024-03-13T22:44:36.492872-06:00'
model: gpt-4-0125-preview
summary: "H\xE3y \u0111\u1EC3 m\u1ED9t \xEDt m\xE3 xu\u1EA5t hi\u1EC7n tr\xEAn m\xE0\
  n h\xECnh."
title: "In ra th\xF4ng tin g\u1EE1 l\u1ED7i"
weight: 33
---

## Làm thế nào:
Hãy để một ít mã xuất hiện trên màn hình:

```java
public class DebugExample {
    public static void main(String[] args) {
        int sum = 0;
        for (int i = 1; i <= 10; i++) {
            sum += i;
            System.out.println("Thêm " + i + ", tổng hiện tại: " + sum);
        }
    }
}
```

Đoạn mã này cộng dồn các số từ 1 đến 10 và in ra tiến trình:

```
Thêm 1, tổng hiện tại: 1
Thêm 2, tổng hiện tại: 3
...
Thêm 10, tổng hiện tại: 55
```

## Sâu hơn
Trước khi các IDE trở nên thông minh, việc sử dụng printf để debug là lựa chọn hàng đầu. Ngay cả bây giờ, giữa những điểm dừng (breakpoints) hoàn thiện, đôi khi một `System.out.println()` được đặt khéo léo là tất cả những gì bạn cần để điều chỉnh mọi thứ.

Các lựa chọn khác? Các frameworks ghi log như Log4J hay SLF4J cho bạn kiểm soát thông tin debug, tách biệt nó khỏi đầu ra hệ thống và cho phép bạn thay đổi độ chi tiết.

Về cài đặt, hãy nhớ rằng `System.out` là một đối tượng `PrintStream`, mặc định là stdout. Nó có thể được thay thế để chuyển hướng đầu ra, làm cho việc kiểm tra hoặc ghi log kém phức tạp hơn.

## Xem thêm
- [Hướng dẫn của Oracle về Luồng I/O](https://docs.oracle.com/javase/tutorial/essential/io/streams.html)
- [Các Phương pháp Tốt nhất cho Việc Ghi Log trong Java](https://www.baeldung.com/java-logging-intro)
- [Tài liệu SLF4J](http://www.slf4j.org/docs.html)
