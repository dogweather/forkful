---
title:                "Xóa các ký tự phù hợp với một mẫu"
date:                  2024-01-28T21:58:45.483290-07:00
model:                 gpt-4-0125-preview
simple_title:         "Xóa các ký tự phù hợp với một mẫu"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/clojure/deleting-characters-matching-a-pattern.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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
