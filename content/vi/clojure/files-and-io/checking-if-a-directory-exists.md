---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:01.622719-07:00
description: "L\xE0m th\u1EBF n\xE0o: S\u1EED d\u1EE5ng `clojure.java.io/file` \u0111\
  \u1EC3 t\u1EA1o m\u1ED9t \u0111\u1ED1i t\u01B0\u1EE3ng File v\xE0 `.exists` \u0111\
  \u1EC3 ki\u1EC3m tra xem n\xF3 c\xF3 t\u1ED3n t\u1EA1i hay kh\xF4ng. Ph\u01B0\u01A1\
  ng th\u1EE9c `isDirectory` x\xE1c\u2026"
lastmod: '2024-03-13T22:44:36.171967-06:00'
model: gpt-4-0125-preview
summary: "S\u1EED d\u1EE5ng `clojure.java.io/file` \u0111\u1EC3 t\u1EA1o m\u1ED9t\
  \ \u0111\u1ED1i t\u01B0\u1EE3ng File v\xE0 `.exists` \u0111\u1EC3 ki\u1EC3m tra\
  \ xem n\xF3 c\xF3 t\u1ED3n t\u1EA1i hay kh\xF4ng."
title: "Ki\u1EC3m tra xem th\u01B0 m\u1EE5c c\xF3 t\u1ED3n t\u1EA1i kh\xF4ng"
weight: 20
---

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
