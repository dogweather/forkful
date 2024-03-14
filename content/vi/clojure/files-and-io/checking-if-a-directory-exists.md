---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:01.622719-07:00
description: "Ki\u1EC3m tra xem m\u1ED9t th\u01B0 m\u1EE5c c\xF3 t\u1ED3n t\u1EA1\
  i kh\xF4ng c\xF3 ngh\u0129a l\xE0 x\xE1c nh\u1EADn li\u1EC7u m\u1ED9t \u0111\u01B0\
  \u1EDDng d\u1EABn c\xF3 tr\u1ECF \u0111\u1EBFn m\u1ED9t th\u01B0 m\u1EE5c tr\xEA\
  n h\u1EC7 th\u1ED1ng t\u1EC7p c\u1EE7a b\u1EA1n kh\xF4ng. L\u1EADp tr\xECnh vi\xEA\
  n\u2026"
lastmod: '2024-03-13T22:44:36.171967-06:00'
model: gpt-4-0125-preview
summary: "Ki\u1EC3m tra xem m\u1ED9t th\u01B0 m\u1EE5c c\xF3 t\u1ED3n t\u1EA1i kh\xF4\
  ng c\xF3 ngh\u0129a l\xE0 x\xE1c nh\u1EADn li\u1EC7u m\u1ED9t \u0111\u01B0\u1EDD\
  ng d\u1EABn c\xF3 tr\u1ECF \u0111\u1EBFn m\u1ED9t th\u01B0 m\u1EE5c tr\xEAn h\u1EC7\
  \ th\u1ED1ng t\u1EC7p c\u1EE7a b\u1EA1n kh\xF4ng. L\u1EADp tr\xECnh vi\xEAn\u2026"
title: "Ki\u1EC3m tra xem th\u01B0 m\u1EE5c c\xF3 t\u1ED3n t\u1EA1i kh\xF4ng"
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
