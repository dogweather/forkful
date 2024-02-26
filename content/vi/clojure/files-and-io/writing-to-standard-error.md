---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:34.712634-07:00
description: "Vi\u1EBFt v\xE0o l\u1ED7i chu\u1EA9n (`stderr`) l\xE0 m\u1ED9t c\xE1\
  ch \u0111\u1EC3 xu\u1EA5t th\xF4ng b\xE1o l\u1ED7i v\xE0 ch\u1EA9n \u0111o\xE1n.\
  \ L\u1EADp tr\xECnh vi\xEAn l\xE0m v\u1EADy \u0111\u1EC3 ph\xE2n bi\u1EC7t ch\xFA\
  ng v\u1EDBi \u0111\u1EA7u ra th\xF4ng th\u01B0\u1EDDng\u2026"
lastmod: '2024-02-25T18:49:34.552642-07:00'
model: gpt-4-0125-preview
summary: "Vi\u1EBFt v\xE0o l\u1ED7i chu\u1EA9n (`stderr`) l\xE0 m\u1ED9t c\xE1ch \u0111\
  \u1EC3 xu\u1EA5t th\xF4ng b\xE1o l\u1ED7i v\xE0 ch\u1EA9n \u0111o\xE1n. L\u1EAD\
  p tr\xECnh vi\xEAn l\xE0m v\u1EADy \u0111\u1EC3 ph\xE2n bi\u1EC7t ch\xFAng v\u1EDB\
  i \u0111\u1EA7u ra th\xF4ng th\u01B0\u1EDDng\u2026"
title: "Ghi v\xE0o l\u1ED7i chu\u1EA9n"
---

{{< edit_this_page >}}

## Gì & Tại sao?
Viết vào lỗi chuẩn (`stderr`) là một cách để xuất thông báo lỗi và chẩn đoán. Lập trình viên làm vậy để phân biệt chúng với đầu ra thông thường (`stdout`), điều này làm cho việc gỡ lỗi và ghi log trở nên dễ dàng hơn.

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
