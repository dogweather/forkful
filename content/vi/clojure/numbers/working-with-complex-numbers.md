---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:37.564289-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Clojure cung c\u1EA5p h\u1ED7 tr\u1EE3\
  \ s\u1EB5n c\xF3 cho s\u1ED1 ph\u1EE9c th\xF4ng qua l\u1EDBp ti\u1EC7n \xEDch `clojure.lang.Numbers`.\
  \ S\u1EED d\u1EE5ng `complex` \u0111\u1EC3 t\u1EA1o s\u1ED1 ph\u1EE9c v\xE0 th\u1EF1\
  c hi\u1EC7n\u2026"
lastmod: '2024-03-13T22:44:36.144883-06:00'
model: gpt-4-0125-preview
summary: "Clojure cung c\u1EA5p h\u1ED7 tr\u1EE3 s\u1EB5n c\xF3 cho s\u1ED1 ph\u1EE9\
  c th\xF4ng qua l\u1EDBp ti\u1EC7n \xEDch `clojure.lang.Numbers`."
title: "L\xE0m vi\u1EC7c v\u1EDBi s\u1ED1 ph\u1EE9c"
weight: 14
---

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
