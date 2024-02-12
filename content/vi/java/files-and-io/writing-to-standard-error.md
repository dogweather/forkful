---
title:                "Ghi vào lỗi chuẩn"
aliases:
- /vi/java/writing-to-standard-error/
date:                  2024-01-28T22:13:36.287951-07:00
model:                 gpt-4-0125-preview
simple_title:         "Ghi vào lỗi chuẩn"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/java/writing-to-standard-error.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Việc ghi vào lỗi chuẩn (stderr) là một cách để xuất thông điệp lỗi và chẩn đoán riêng biệt khỏi đầu ra chuẩn (stdout). Các lập trình viên sử dụng nó để báo hiệu rằng điều gì đó ngoại lệ đã xảy ra, giúp dễ dàng gỡ lỗi và cô lập vấn đề.

## Làm thế nào:

Java làm cho việc ghi vào stderr trở nên đơn giản sử dụng `System.err`. Dưới đây là một cái nhìn nhanh:

```java
public class StderrExample {
    public static void main(String[] args) {
        System.err.println("Error: Something went wrong!");
    }
}
```

Chạy cái này bạn sẽ nhận được:

```
Error: Something went wrong!
```

Lưu ý: Trong khi stdout thường đi đến bảng điều khiển, stderr có thể được chuyển hướng đến một tệp hoặc địa điểm khác, giữ cho các thông điệp lỗi được tách biệt.

## Sâu hơn

Trong lịch sử của các hệ thống giống Unix, stderr là bộ điều khiển tệp số 2, khác biệt với stdout (bộ điều khiển tệp số 1). Điều này cho phép xử lý và chuyển hướng khác nhau. Các lựa chọn thay thế cho `System.err` bao gồm các khuôn khổ nhật ký như Log4J hoặc SLF4J, cung cấp nhiều tính năng hơn. Trong Java, stderr được thực hiện trong lớp `System` và là một thể hiện của `PrintStream`. Nó không được đệm, có nghĩa là đầu ra tức thì.

## Xem thêm

- [Tài liệu Oracle Java - System.err](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html#err)
- [Wikipedia - Dòng chuẩn](https://en.wikipedia.org/wiki/Standard_streams)
- [Hướng dẫn về Nhật ký Java](https://www.baeldung.com/java-logging-intro)
