---
title:                "Tạo một tập tin tạm thời"
date:                  2024-01-28T21:58:27.281008-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tạo một tập tin tạm thời"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/clojure/creating-a-temporary-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Tạo một tệp tạm thời là quá trình tạo một tệp ngắn hạn cho việc lưu trữ dữ liệu trung gian. Lập trình viên sử dụng chúng cho các mục đích như lưu trữ tạm thời, xử lý dữ liệu, hoặc khi không muốn làm đầy bộ nhớ lưu trữ vĩnh viễn.

## Làm thế nào:
Clojure làm cho việc này trở nên đơn giản. Thư viện `clojure.java.io` đã hỗ trợ bạn.

```Clojure
(require '[clojure.java.io :as io])

; Tạo một tệp tạm
(def temp-file (io/file (io/create-temp-file "prefix-" ".txt")))

; Sử dụng tệp tạm
(spit temp-file "Dữ liệu tạm thời là tạm thời")

; Kiểm tra nội dung
(println (slurp temp-file)) ; => "Dữ liệu tạm thời là tạm thời"

; Dọn dẹp bằng cách xóa tệp tạm khi bạn hoàn tất
(.delete temp-file)
```

Không có gì tồn tại mãi mãi. Dữ liệu tạm thời của chúng tôi giờ đây đã được yên nghỉ.

## Sâu hơn
Ý tưởng về tệp tạm thời đã tồn tại từ những ngày đầu của việc tính toán, chủ yếu để tránh sử dụng hết bộ nhớ lưu trữ chính hạn chế. Nó giống như việc thuê không gian số hóa tạm thời.

Clojure dựa vào vai trò của Java ở đây, sử dụng khả năng của lớp `File` của Java. Mặc dù bạn có thể lặn sâu trực tiếp vào rừng Java, Clojure đã đóng gói nó một cách gọn gàng.

Có lựa chọn khác? Chắc chắn rồi. Thư mục tạm cũng là một cái gì đó. Nhưng đó là một câu chuyện khác, và Clojure cũng đã đề cập đến điều đó (xem `create-temp-dir`).

Tại sao không chỉ sử dụng bộ nhớ? Có, tệp tạm hoàn hảo cho việc xử lý dữ liệu quá lớn cho RAM hoặc khi bạn muốn có một tệp vật lý mà không phải lo lắng về việc lưu trữ lâu dài hay dọn dẹp.

## Xem Thêm
- [Tài liệu IO của Clojure](https://clojure.github.io/clojure/clojure.java.io-api.html)
- [Tài liệu File của Java](https://docs.oracle.com/javase/7/docs/api/java/io/File.html) — cho những chi tiết cơ bản.
- Có thể bạn muốn khám phá [gói tệp NIO của Java](https://docs.oracle.com/javase/8/docs/api/java/nio/file/package-summary.html) cho các thao tác tệp lớn và phức tạp hơn ngoài những điều cơ bản.