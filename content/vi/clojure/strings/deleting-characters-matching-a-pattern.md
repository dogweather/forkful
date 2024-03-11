---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:45.483290-07:00
description: "X\xF3a c\xE1c k\xFD t\u1EF1 ph\xF9 h\u1EE3p v\u1EDBi m\u1ED9t m\u1EAB\
  u ngh\u0129a l\xE0 lo\u1EA1i b\u1ECF c\xE1c chu\u1ED7i c\u1EE5 th\u1EC3 kh\u1ECF\
  i m\u1ED9t chu\u1ED7i k\xFD t\u1EF1. L\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1\
  u n\xE0y \u0111\u1EC3 l\xE0m s\u1EA1ch d\u1EEF li\u1EC7u, th\u1EF1c thi c\xE1c\u2026"
lastmod: '2024-03-11T00:14:09.377172-06:00'
model: gpt-4-0125-preview
summary: "X\xF3a c\xE1c k\xFD t\u1EF1 ph\xF9 h\u1EE3p v\u1EDBi m\u1ED9t m\u1EABu ngh\u0129\
  a l\xE0 lo\u1EA1i b\u1ECF c\xE1c chu\u1ED7i c\u1EE5 th\u1EC3 kh\u1ECFi m\u1ED9t\
  \ chu\u1ED7i k\xFD t\u1EF1. L\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0\
  y \u0111\u1EC3 l\xE0m s\u1EA1ch d\u1EEF li\u1EC7u, th\u1EF1c thi c\xE1c\u2026"
title: "X\xF3a c\xE1c k\xFD t\u1EF1 ph\xF9 h\u1EE3p v\u1EDBi m\u1ED9t m\u1EABu"
---

{{< edit_this_page >}}

## Gì & Tại Sao?
Xóa các ký tự phù hợp với một mẫu nghĩa là loại bỏ các chuỗi cụ thể khỏi một chuỗi ký tự. Lập trình viên làm điều này để làm sạch dữ liệu, thực thi các định dạng, hoặc loại bỏ thông tin không mong muốn.

## Làm thế nào:

Để xóa các ký tự sử dụng một mẫu trong Clojure, bạn sử dụng biểu thức chính quy với các hàm `re-seq`, `re-find`, hoặc `re-matches` kết hợp với `clojure.string/replace`.

```Clojure
(require '[clojure.string :as str])

;; Xóa tất cả các chữ số từ một chuỗi
(str/replace "He110 W0rld" #"\d+" "")
;; => "He Wrd"

;; Xóa các ký tự đặc biệt cụ thể
(str/replace "Hello, World! #Clojure" #"[,!#]" "")
;; => "Hello World Clojure"

;; Chỉ giữ lại các ký tự từ và khoảng trắng
(str/replace "Email@Example.com" #"[^\w\s]+" "")
;; => "EmailExamplecom"
```

## Sâu Hơn
Clojure, phản ánh di sản Lisp của mình, xuất sắc trong việc xử lý biểu tượng, làm cho việc khớp mẫu trở nên dễ dàng. Được giới thiệu vào năm 2007, nó xây dựng dựa trên khả năng của Java Virtual Machine (JVM), sử dụng lớp `Pattern` mạnh mẽ của Java cho biểu thức chính quy.

Các phương pháp thay thế cho biểu thức chính quy bao gồm việc lặp qua và thao tác chuỗi một cách thủ công, nhưng những phương pháp này thường rườm rà và dễ mắc lỗi hơn. Các thư viện như `clojure.spec` có thể giúp xác thực và điều chỉnh dữ liệu theo mẫu mà không cần xóa trực tiếp.

Các thao tác xóa thường rất hiệu quả, nhưng cần chú ý đến độ phức tạp của biểu thức chính quy, có thể biến một nhiệm vụ O(n) trở nên tồi tệ hơn nhiều. Chuỗi bất biến của Clojure có nghĩa là mỗi lần `replace` sẽ tạo ra một chuỗi mới, điều này đáng được xem xét cho các ứng dụng nhạy cảm với bộ nhớ.

## Xem Thêm
- [API chuỗi của Clojure](https://clojure.github.io/clojure/clojure.string-api.html)
- [Lớp Pattern của Java](https://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html)
- [Regular-Expressions.info](https://www.regular-expressions.info/)
- [clojure.spec](https://clojure.org/guides/spec)
