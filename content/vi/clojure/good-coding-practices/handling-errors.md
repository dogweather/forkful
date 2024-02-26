---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:42.268671-07:00
description: "X\u1EED l\xFD l\u1ED7i l\xE0 v\u1EC1 vi\u1EC7c qu\u1EA3n l\xFD nh\u1EEF\
  ng t\xECnh hu\u1ED1ng kh\xF4ng mong \u0111\u1EE3i trong ch\u01B0\u01A1ng tr\xEC\
  nh - gi\u1ED1ng nh\u01B0 m\u1ED9t v\u1EC7 s\u0129 gi\u1EA3i quy\u1EBFt r\u1EAFc\
  \ r\u1ED1i. L\u1EADp tr\xECnh vi\xEAn th\xEDch s\u1EF1 tr\u01A1n\u2026"
lastmod: '2024-02-25T18:49:34.539961-07:00'
model: gpt-4-0125-preview
summary: "X\u1EED l\xFD l\u1ED7i l\xE0 v\u1EC1 vi\u1EC7c qu\u1EA3n l\xFD nh\u1EEF\
  ng t\xECnh hu\u1ED1ng kh\xF4ng mong \u0111\u1EE3i trong ch\u01B0\u01A1ng tr\xEC\
  nh - gi\u1ED1ng nh\u01B0 m\u1ED9t v\u1EC7 s\u0129 gi\u1EA3i quy\u1EBFt r\u1EAFc\
  \ r\u1ED1i. L\u1EADp tr\xECnh vi\xEAn th\xEDch s\u1EF1 tr\u01A1n\u2026"
title: "X\u1EED l\xFD l\u1ED7i"
---

{{< edit_this_page >}}

## Thế nào và Tại sao?
Xử lý lỗi là về việc quản lý những tình huống không mong đợi trong chương trình - giống như một vệ sĩ giải quyết rắc rối. Lập trình viên thích sự trơn tru; xử lý lỗi giúp giữ cho rắc rối trong tầm kiểm soát, đảm bảo mã của họ không vấp ngã khi đối mặt với những tình huống không mong đợi.

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
