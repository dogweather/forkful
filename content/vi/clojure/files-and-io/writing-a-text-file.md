---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:36.405998-07:00
description: "Vi\u1EC7c vi\u1EBFt m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n bao g\u1ED3\
  m vi\u1EC7c t\u1EA1o ho\u1EB7c thay \u0111\u1ED5i d\u1EEF li\u1EC7u v\u0103n b\u1EA3\
  n v\xE0 l\u01B0u n\xF3 v\xE0o m\u1ED9t t\u1EC7p tr\xEAn ph\u01B0\u01A1ng ti\u1EC7\
  n l\u01B0u tr\u1EEF c\u1EE7a b\u1EA1n. L\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7\
  n\u2026"
lastmod: '2024-03-11T00:14:09.424638-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c vi\u1EBFt m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n bao g\u1ED3m vi\u1EC7\
  c t\u1EA1o ho\u1EB7c thay \u0111\u1ED5i d\u1EEF li\u1EC7u v\u0103n b\u1EA3n v\xE0\
  \ l\u01B0u n\xF3 v\xE0o m\u1ED9t t\u1EC7p tr\xEAn ph\u01B0\u01A1ng ti\u1EC7n l\u01B0\
  u tr\u1EEF c\u1EE7a b\u1EA1n. L\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n\u2026"
title: "Vi\u1EBFt m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Việc viết một tệp văn bản bao gồm việc tạo hoặc thay đổi dữ liệu văn bản và lưu nó vào một tệp trên phương tiện lưu trữ của bạn. Lập trình viên thực hiện điều này để ghi nhật ký dữ liệu, cài đặt cấu hình, hoặc xuất báo cáo dễ đọc.

## Cách thực hiện:

Trong Clojure, bạn sử dụng hàm `spit` để viết dữ liệu vào một tệp văn bản. Nó rất đơn giản:

```clojure
(spit "example.txt" "Hello, World! This is Clojure speaking.")
```

Hàm `spit` nhận tên tệp và nội dung. Để thêm nội dung, thiết lập cờ `append`:

```clojure
(spit "example.txt" "\nLet's add this new line." :append true)
```

Kết quả mẫu cho `example.txt` sau cả hai thao tác:

```
Hello, World! This is Clojure speaking.
Let's add this new line.
```

## Sâu hơn nữa

Hàm `spit` của Clojure đến từ thư viện "I/O" của nó - một người kế nhiệm xứng đáng của Lisp với truyền thống về các thao tác tệp ngắn gọn. Các lựa chọn khác trong Clojure bao gồm `clojure.java.io/writer` cho viết đệm và các thư viện như `slurp` để đọc tệp. Khi sử dụng `spit`, hãy nhớ rằng nó không dành cho các luồng dữ liệu lớn do vấn đề về bộ nhớ tiềm ẩn - sử dụng `writer` và lặp qua dữ liệu thay thế.

## Xem thêm

- Tài liệu Clojure cho `spit`: [https://clojuredocs.org/clojure.core/spit](https://clojuredocs.org/clojure.core/spit)
- Bọc `java.io` của Clojure: [https://clojure.github.io/clojure/clojure.java.io-api.html](https://clojure.github.io/clojure/clojure.java.io-api.html)
