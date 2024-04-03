---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:42.268671-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Clojure, gi\u1ED1ng nh\u01B0 t\u1ED5 ti\xEA\
  n Lisp c\u1EE7a m\xECnh, d\u1EF1a v\xE0o ngo\u1EA1i l\u1EC7 \u0111\u1EC3 x\u1EED\
  \ l\xFD l\u1ED7i. D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch b\u1EA1n th\u1EC3 hi\u1EC7\
  n b\u1EA3n th\xE2n khi m\u1ECDi th\u1EE9 tr\u1EDF n\xEAn t\u1ED3i\u2026"
lastmod: '2024-03-13T22:44:36.163002-06:00'
model: gpt-4-0125-preview
summary: "Clojure, gi\u1ED1ng nh\u01B0 t\u1ED5 ti\xEAn Lisp c\u1EE7a m\xECnh, d\u1EF1\
  a v\xE0o ngo\u1EA1i l\u1EC7 \u0111\u1EC3 x\u1EED l\xFD l\u1ED7i."
title: "X\u1EED l\xFD l\u1ED7i"
weight: 16
---

## Cách thực hiện:
Clojure, giống như tổ tiên Lisp của mình, dựa vào ngoại lệ để xử lý lỗi. Dưới đây là cách bạn thể hiện bản thân khi mọi thứ trở nên tồi tệ.

Ném một ngoại lệ là điều dễ dàng:
```Clojure
(throw (Exception. "Ối! Có gì đó sai sai."))
```

Bắt một ngoại lệ, bạn sẽ làm điều này rất nhiều:
```Clojure
(try
  ;; mã có rủi ro
  (/ 1 0)
  (catch ArithmeticException e
    (println "Không thể chia cho không!"))
  ;; khối finally chạy bất kể điều gì xảy ra
  (finally 
    (println "Mã dọn dẹp được đặt ở đây.")))
```
Đầu ra mẫu cho khối catch phía trên:
```
Không thể chia cho không!
Mã dọn dẹp được đặt ở đây.
```

Sử dụng `ex-info` và `ex-data` để cung cấp ngữ cảnh phong phú hơn về ngoại lệ:
```Clojure
(try
  ;; gây ra một ngoại lệ tự tạo
  (throw (ex-info "Lỗi tự tạo" {:loại :lỗi-tùy-chỉnh}))
  (catch Exception e
    ;; lấy dữ liệu từ ngoại lệ tự tạo của chúng ta
    (println (ex-data e))))
```
Đầu ra mẫu:
```
{:loại :lỗi-tùy-chỉnh}
```

## Sâu hơn
Câu chuyện xử lý lỗi trong Clojure không có gì khác biệt ngoại lệ so với các Lisps khác hay thậm chí Java (từ đó nó kế thừa cơ chế `try-catch`). Nó thực dụng; sử dụng ngoại lệ là con đường chính, giống như Java, nhưng Clojure cung cấp một hương vị hàm với `ex-info` và `ex-data` cho dữ liệu lỗi phong phú hơn.

Các phương án thay thế cho xử lý lỗi trong Clojure bao gồm sử dụng các cấu trúc monadic, như monad `either` từ các thư viện như `cats`, hoặc core.async cho việc truyền bá lỗi dựa trên kênh. Tuy nhiên, những điều này phức tạp hơn và được sử dụng trong các tình huống cụ thể.

Lịch sử, xử lý lỗi trong các ngôn ngữ lập trình đã phát triển từ việc trả về trạng thái đơn giản đến các cơ chế xử lý ngoại lệ phức tạp hơn của các ngôn ngữ hiện đại. Clojure chọn sự đơn giản và một chút hương vị lập trình hàm, kết hợp cũ và mới.

## Xem thêm
- Hướng dẫn về ngoại lệ của Clojure: https://clojure.org/guides/exceptions
- Thư viện “Cats” cho các cách tiếp cận hàm hóa hơn: https://github.com/funcool/cats
- “Core.async” cho lập trình bất đồng bộ: https://github.com/clojure/core.async
