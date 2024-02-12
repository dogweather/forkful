---
title:                "Đọc một tệp văn bản"
date:                  2024-01-28T22:04:53.696607-07:00
model:                 gpt-4-0125-preview
simple_title:         "Đọc một tệp văn bản"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/clojure/reading-a-text-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Đọc một tệp văn bản có nghĩa là lấy dữ liệu từ một tệp được lưu trữ trên ổ đĩa của bạn vào chương trình của bạn. Các lập trình viên làm điều này để xử lý hoặc phân tích nội dung mà không cần nhập liệu thủ công, tự động hóa các tác vụ, hoặc phân tích cú pháp dữ liệu cấu hình.

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
