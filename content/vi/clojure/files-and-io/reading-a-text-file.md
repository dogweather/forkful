---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:53.696607-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Truy\u1EC1n th\u1ED1ng, vi\u1EC7c \u0111\
  \u1ECDc t\u1EC7p trong c\xE1c ng\xF4n ng\u1EEF l\u1EADp tr\xECnh l\xE0 m\u1ED9t\
  \ nhi\u1EC7m v\u1EE5 d\xE0i d\xF2ng v\u1EDBi nhi\u1EC1u b\u01B0\u1EDBc \u0111\u1EC3\
  \ x\u1EED l\xFD l\u1ED7i v\xE0 t\xE0i nguy\xEAn. V\u1EDBi Clojure,\u2026"
lastmod: '2024-04-05T21:53:37.599006-06:00'
model: gpt-4-0125-preview
summary: "Truy\u1EC1n th\u1ED1ng, vi\u1EC7c \u0111\u1ECDc t\u1EC7p trong c\xE1c ng\xF4\
  n ng\u1EEF l\u1EADp tr\xECnh l\xE0 m\u1ED9t nhi\u1EC7m v\u1EE5 d\xE0i d\xF2ng v\u1EDB\
  i nhi\u1EC1u b\u01B0\u1EDBc \u0111\u1EC3 x\u1EED l\xFD l\u1ED7i v\xE0 t\xE0i nguy\xEA\
  n."
title: "\u0110\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n"
weight: 22
---

## Cách thực hiện:
```Clojure
;; Đọc toàn bộ tệp dưới dạng chuỗi
(slurp "example.txt")

;; Kết quả: "Hello, this is your file content!"

;; Đọc tệp theo từng dòng
(with-open [reader (clojure.java.io/reader "example.txt")]
  (doseq [line (line-seq reader)]
    (println line)))

;; Kết quả:
;; Hello,
;; this is your
;; file content!
```

## Sâu hơn nữa
Truyền thống, việc đọc tệp trong các ngôn ngữ lập trình là một nhiệm vụ dài dòng với nhiều bước để xử lý lỗi và tài nguyên. Với Clojure, bạn được hưởng lợi từ hàm `slurp`, một dòng lệnh đơn giản tinh tế để nắm bắt toàn bộ nội dung của tệp. Đối với việc đọc từng dòng, `line-seq` kết hợp với `with-open` đảm bảo việc xử lý tệp một cách hiệu quả và an toàn. Cũng đáng chú ý là dù `slurp` rất tiện lợi, nó không lý tưởng cho các tệp lớn do ràng buộc về bộ nhớ. Đó là khi `line-seq` tỏa sáng, khi nó đọc tệp một cách biếng (lazy), từng dòng một.

Các phương án thay thế cho việc đọc tệp trong Clojure bao gồm sử dụng `clojure.java.io/file` với các hàm như `reader` và cấu trúc như `with-open` để quản lý tay điều khiển tệp một cách thủ công. Sự cân nhắc ở đây là giữa sự tiện lợi (dùng `slurp`) và kiểm soát chính xác kết hợp với sự an toàn tài nguyên (`with-open` và `reader`).

Về mặt triển khai, phương pháp tiếp cận của Clojure dựa trên các lớp IO của Java, do đó khi bạn đang xử lý tệp trong Clojure, bạn đang sử dụng các thư viện của Java đã được kiểm nghiệm, chín chắn, bọc trong một ngữ cảnh hàm. Luôn chú ý đến tài nguyên: các tệp mở tiêu thụ cả tay cầm và bộ nhớ, vì vậy việc xử lý tệp sạch sẽ là một thói quen tốt.

## Xem thêm
- Tài liệu Clojure cho `slurp`: https://clojuredocs.org/clojure.core/slurp
- Tài liệu Clojure cho `line-seq`: https://clojuredocs.org/clojure.core/line-seq
- Tương tác với Java trong Clojure: https://clojure.org/reference/java_interop
- Làm việc với tệp trong Clojure (Practical.li): https://practical.li/clojure/working-with-files.html
