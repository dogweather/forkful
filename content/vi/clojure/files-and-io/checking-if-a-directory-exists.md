---
title:                "Kiểm tra xem thư mục có tồn tại không"
aliases:
- /vi/clojure/checking-if-a-directory-exists/
date:                  2024-01-28T21:56:01.622719-07:00
model:                 gpt-4-0125-preview
simple_title:         "Kiểm tra xem thư mục có tồn tại không"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/clojure/checking-if-a-directory-exists.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Kiểm tra xem một thư mục có tồn tại không có nghĩa là xác nhận liệu một đường dẫn có trỏ đến một thư mục trên hệ thống tệp của bạn không. Lập trình viên làm điều này để ngăn chặn lỗi, đảm bảo xử lý tệp chính xác và thiết lập các điều kiện cần thiết trước khi thực hiện các thao tác tệp.

## Làm thế nào:
Sử dụng `clojure.java.io/file` để tạo một đối tượng File và `.exists` để kiểm tra xem nó có tồn tại hay không. Phương thức `isDirectory` xác nhận liệu File có phải là một thư mục không.

```Clojure
(require '[clojure.java.io :as io])

(defn directory-exists? [path]
  (let [dir (io/file path)]
    (and (.exists dir) (.isDirectory dir))))

;; Ví dụ sử dụng:
(directory-exists? "/path/to/directory") ;=> true hoặc false
```
Kết quả Mẫu:
```
true ; nếu thư mục tồn tại
false ; nếu thư mục không tồn tại
```

## Khám phá sâu hơn
Trong lịch sử, một quy trình tương tự được sử dụng trong Java; vì Clojure chạy trên JVM, nó tận dụng các thư viện Java cho các hoạt động hệ thống tệp. Các phương án thay thế trong Clojure có thể bao gồm việc sử dụng các chức năng hoặc thư viện Java khác như `nio.file.Files`. Phía sau, việc kiểm tra tồn tại của một thư mục có thể tiêu tốn nhiều IO và có thể hoạt động khác nhau giữa các hệ điều hành và quyền hệ thống tệp, đó là lý do tại sao việc đảm bảo sự tồn tại của nó trước khi thực hiện thêm các thao tác là rất quan trọng.

## Xem thêm
- Tài liệu Clojure về I/O: [https://clojure.github.io/clojure/clojure.java.io-api.html](https://clojure.github.io/clojure/clojure.java.io-api.html)
- Lớp File của Java: [https://docs.oracle.com/javase/8/docs/api/java/io/File.html](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- Lớp Files NIO của Java: [https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html)
