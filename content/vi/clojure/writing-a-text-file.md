---
title:                "Viết một tệp văn bản"
date:                  2024-01-28T22:12:36.405998-07:00
model:                 gpt-4-0125-preview
simple_title:         "Viết một tệp văn bản"

category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/clojure/writing-a-text-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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
