---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:34.712634-07:00
description: "L\xE0m th\u1EBF n\xE0o: \u0110\u1EC3 vi\u1EBFt v\xE0o l\u1ED7i chu\u1EA9\
  n trong Clojure, b\u1EA1n s\u1EBD s\u1EED d\u1EE5ng `binding` v\u1EDBi `*err*`.\
  \ D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9t v\xED d\u1EE5 nhanh."
lastmod: '2024-03-13T22:44:36.174672-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1EC3 vi\u1EBFt v\xE0o l\u1ED7i chu\u1EA9n trong Clojure, b\u1EA1\
  n s\u1EBD s\u1EED d\u1EE5ng `binding` v\u1EDBi `*err*`."
title: "Ghi v\xE0o l\u1ED7i chu\u1EA9n"
weight: 25
---

## Làm thế nào:
Để viết vào lỗi chuẩn trong Clojure, bạn sẽ sử dụng `binding` với `*err*`. Dưới đây là một ví dụ nhanh:

```Clojure
(binding [*err* *out*]
  (println "Điều này sẽ đi đến lỗi chuẩn"))
```

Mẫu đầu ra (trong shell của bạn):

```
$ clj your_script.clj 2> error.log
$ cat error.log
Điều này sẽ đi đến lỗi chuẩn
```

Đoạn mã này liên kết `*err*` với `*out*`, là đầu ra chuẩn, để bạn có thể thấy những gì thường sẽ đi đến `stderr`.

## Đào Sâu
Theo lịch sử, các hệ thống Unix có hai luồng đầu ra riêng biệt, `stdout` và `stderr`, cho các loại dữ liệu khác nhau. Trong Clojure, `*out*` đề cập đến `stdout` và `*err*` đến `stderr`. Các lựa chọn thay thế cho `binding` bao gồm sử dụng Java interop trực tiếp (ví dụ, `(.println System/err "tin nhắn")`). Về mặt triển khai, `*err*` là một var động, cho phép liên kết cục bộ theo thread - một điểm tinh tế có thể ảnh hưởng đến cách lỗi được ghi trong các ứng dụng đồng thời.

## Xem Thêm
- Tài liệu Clojure về `*err*`: https://clojuredocs.org/clojure.core/*err*
- Tài liệu Clojure về `binding`: https://clojuredocs.org/clojure.core/binding
- API Java cho `PrintStream` (mà `System/err` là): https://docs.oracle.com/javase/8/docs/api/java/io/PrintStream.html

Để hiểu rõ hơn về các luồng chuẩn, những tài liệu sau cũng có thể hữu ích:
- Wikipedia về Luồng Chuẩn: https://en.wikipedia.org/wiki/Standard_streams
- Tài liệu về luồng chuẩn Unix: `man stdio`
