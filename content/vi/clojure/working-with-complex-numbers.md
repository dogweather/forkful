---
title:                "Làm việc với số phức"
aliases:
- vi/clojure/working-with-complex-numbers.md
date:                  2024-01-28T22:12:37.564289-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với số phức"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/clojure/working-with-complex-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Làm Thế Nào & Tại Sao?
Số phức mở rộng số thực với một phần bổ sung, đơn vị ảo 'i'. Các lập trình viên sử dụng chúng trong nhiều lĩnh vực, bao gồm xử lý tín hiệu, lý thuyết điện từ, và fractal, nơi mà các phép tính liên quan đến căn bậc hai của một số âm trở nên thông thường.

## Cách thực hiện:
Clojure cung cấp hỗ trợ sẵn có cho số phức thông qua lớp tiện ích `clojure.lang.Numbers`. Sử dụng `complex` để tạo số phức và thực hiện các phép toán.

```clojure
;; Tạo số phức
(def a (clojure.lang.Numbers/complex 3 4))  ; 3 + 4i
(def b (clojure.lang.Numbers/complex 1 -1)) ; 1 - i

;; Phép cộng
(+ a b) ;=> #object[clojure.lang.Numbers.Complex 0x5c6cfe9 "4 + 3i"]

;; Phép trừ
(- a b) ;=> #object[clojure.lang.Numbers.Complex 0x5e51118 "2 + 5i"]

;; Phép nhân
(* a b) ;=> #object[clojure.lang.Numbers.Complex 0x6ec3f0df "7 + i"]

;; Phép chia
(/ a b) ;=> #object[clojure.lang.Numbers.Complex 0x5db0cd10 "3.5 + 3.5i"]

;; Số phức liên hợp
(.conjugate a) ;=> #object[clojure.lang.Numbers.Complex 0x47c6e076 "3 - 4i"]
```

## Chuyên Sâu
Số phức đã được các nhà toán học như Gauss và Euler chính thức hóa vào thế kỷ 18. Mặc dù ban đầu gặp phải sự hoài nghi, chúng kể từ đó đã trở nên quan trọng trong khoa học và kỹ thuật hiện đại. Clojure không có một kiểu số phức nguyên thủy như một số ngôn ngữ khác (ví dụ, Python), nhưng sự tương tác với Java được tích hợp có thể xử lý các thao tác cần thiết thông qua lớp `clojure.lang.Numbers`.

`java.lang.Complex` của Java là một lựa chọn mạnh mẽ hơn, cung cấp nhiều tính năng và khả năng tối ưu hóa tiềm năng. Sự tương tác với hệ thống máy chủ của Clojure giúp việc làm việc với các thư viện Java trở nên dễ dàng.

Ở bên dưới cùng, toán học số phức liên quan đến việc cộng và nhân các phần thực và ảo, với quy tắc chính là `i^2 = -1`. Phép chia số phức có thể phức tạp hơn, thường yêu cầu sử dụng số liên hợp để tránh chia cho số phức.

## Xem Thêm
- ClojureDocs, cho một tài liệu tham khảo nhanh: https://clojuredocs.org/
- API Java cho `java.lang.Complex`: https://docs.oracle.com/javase/8/docs/api/java/lang/Complex.html
- Trang Wikipedia về số phức cho những người ham hiểu biết toán học: https://en.wikipedia.org/wiki/Complex_number
