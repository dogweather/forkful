---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:23.740317-07:00
description: "In ra \u0111\u1EA7u ra \u0111\u1EC3 g\u1EE1 l\u1ED7i l\xE0 vi\u1EC7\
  c n\xE9m nh\u1EEFng m\u1EA9u th\xF4ng tin nh\u1ECF v\xE0o b\u1EA3ng \u0111i\u1EC1\
  u khi\u1EC3n \u0111\u1EC3 t\xECm v\xE0 x\u1EED l\xFD l\u1ED7i. N\xF3 nhanh, kh\xF4\
  ng c\u1EA7n s\u1EA1ch s\u1EBD, v\xE0 hi\u1EC7u qu\u1EA3 \u0111\u1EC3 hi\u1EC3u nh\u1EEF\
  ng\u2026"
lastmod: '2024-02-25T18:49:34.832207-07:00'
model: gpt-4-0125-preview
summary: "In ra \u0111\u1EA7u ra \u0111\u1EC3 g\u1EE1 l\u1ED7i l\xE0 vi\u1EC7c n\xE9\
  m nh\u1EEFng m\u1EA9u th\xF4ng tin nh\u1ECF v\xE0o b\u1EA3ng \u0111i\u1EC1u khi\u1EC3\
  n \u0111\u1EC3 t\xECm v\xE0 x\u1EED l\xFD l\u1ED7i. N\xF3 nhanh, kh\xF4ng c\u1EA7\
  n s\u1EA1ch s\u1EBD, v\xE0 hi\u1EC7u qu\u1EA3 \u0111\u1EC3 hi\u1EC3u nh\u1EEFng\u2026"
title: "In ra th\xF4ng tin g\u1EE1 l\u1ED7i"
---

{{< edit_this_page >}}

## Gì & Tại sao?
In ra đầu ra để gỡ lỗi là việc ném những mẩu thông tin nhỏ vào bảng điều khiển để tìm và xử lý lỗi. Nó nhanh, không cần sạch sẽ, và hiệu quả để hiểu những gì đang xảy ra bên trong mã của bạn khi nó đang hoạt động một cách hoang dã.

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
