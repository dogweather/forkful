---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:36.287951-07:00
description: "L\xE0m th\u1EBF n\xE0o: Java l\xE0m cho vi\u1EC7c ghi v\xE0o stderr\
  \ tr\u1EDF n\xEAn \u0111\u01A1n gi\u1EA3n s\u1EED d\u1EE5ng `System.err`. D\u01B0\
  \u1EDBi \u0111\xE2y l\xE0 m\u1ED9t c\xE1i nh\xECn nhanh."
lastmod: '2024-03-13T22:44:36.510717-06:00'
model: gpt-4-0125-preview
summary: "Java l\xE0m cho vi\u1EC7c ghi v\xE0o stderr tr\u1EDF n\xEAn \u0111\u01A1\
  n gi\u1EA3n s\u1EED d\u1EE5ng `System.err`."
title: "Ghi v\xE0o l\u1ED7i chu\u1EA9n"
weight: 25
---

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
