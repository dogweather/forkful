---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:36.405998-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Trong Clojure, b\u1EA1n s\u1EED d\u1EE5\
  ng h\xE0m `spit` \u0111\u1EC3 vi\u1EBFt d\u1EEF li\u1EC7u v\xE0o m\u1ED9t t\u1EC7\
  p v\u0103n b\u1EA3n. N\xF3 r\u1EA5t \u0111\u01A1n gi\u1EA3n."
lastmod: '2024-03-13T22:44:36.177442-06:00'
model: gpt-4-0125-preview
summary: "Trong Clojure, b\u1EA1n s\u1EED d\u1EE5ng h\xE0m `spit` \u0111\u1EC3 vi\u1EBF\
  t d\u1EEF li\u1EC7u v\xE0o m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n."
title: "Vi\u1EBFt m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n"
weight: 24
---

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
