---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:57.934346-07:00
description: "N\u1ED9i suy chu\u1ED7i cho ph\xE9p ch\xFAng ta n\xE9m bi\u1EBFn v\xE0\
  o trong chu\u1ED7i m\xE0 kh\xF4ng m\u1EA5t c\xF4ng s\u1EE9c. T\u1EA1i sao? \u0110\
  \u1EC3 x\xE2y d\u1EF1ng v\u0103n b\u1EA3n m\u1ED9t c\xE1ch linh ho\u1EA1t\u2014\
  ti\u1EC7n l\u1EE3i h\u01A1n nhi\u1EC1u so v\u1EDBi\u2026"
lastmod: '2024-03-11T00:14:09.379914-06:00'
model: gpt-4-0125-preview
summary: "N\u1ED9i suy chu\u1ED7i cho ph\xE9p ch\xFAng ta n\xE9m bi\u1EBFn v\xE0o\
  \ trong chu\u1ED7i m\xE0 kh\xF4ng m\u1EA5t c\xF4ng s\u1EE9c. T\u1EA1i sao? \u0110\
  \u1EC3 x\xE2y d\u1EF1ng v\u0103n b\u1EA3n m\u1ED9t c\xE1ch linh ho\u1EA1t\u2014\
  ti\u1EC7n l\u1EE3i h\u01A1n nhi\u1EC1u so v\u1EDBi\u2026"
title: "N\u1ED9i suy chu\u1ED7i k\xFD t\u1EF1"
---

{{< edit_this_page >}}

## Điều gì và Tại sao?
Nội suy chuỗi cho phép chúng ta ném biến vào trong chuỗi mà không mất công sức. Tại sao? Để xây dựng văn bản một cách linh hoạt—tiện lợi hơn nhiều so với cách ghép chuỗi kiểu cũ.

## Làm thế nào:
```Clojure
;; Cơ bản với `str` và `format`
(def name "World")
(str "Xin chào, " name "!")  ; => "Xin chào, World!"

;; Sử dụng `format`, tương tự như định dạng kiểu printf
(format "Tạm biệt, %s!" name)  ; => "Tạm biệt, World!"

;; Clojure không có nội suy chuỗi tích hợp sẵn như các ngôn ngữ khác,
;; nhưng chúng ta có thể sáng tạo với `str` và `format`.
```

## Sâu hơn nữa:
Clojure hơi khắc nghiệt: không có nội suy chuỗi tích hợp sẵn. Tuy nhiên, `str` và `format` là lựa chọn hàng đầu cho chuỗi động. Câu chuyện nguồn gốc? Tinh thần đơn giản của Clojure. Nó tin rằng chúng ta có thể tự xử lý việc xây dựng chuỗi.

Đối với các lựa chọn thay thế, hãy tham gia vào thế giới mẫu: `clostache` (một triển khai Clojure của Mustache) hoặc `hiccup` cho các ngữ cảnh HTML. Chúng trở nên tiện lợi khi `str` và `format` cảm thấy quá đơn giản.

Về phần cốt lõi, `format` ủy quyền cho `String.format` của Java, một sự thật minh họa cho siêu năng lực tương thích Java của Clojure. Vì vậy, mặc dù bạn không có được đường viền, bạn vẫn có sức mạnh của Java khi bạn cần nó.

## Xem thêm:
- Tài liệu Clojure về `str`: https://clojuredocs.org/clojure.core/str
- Tài liệu Clojure về `format`: https://clojuredocs.org/clojure.core/format
- kho GitHub của clostache: https://github.com/fhd/clostache
- kho GitHub của hiccup: https://github.com/weavejester/hiccup
