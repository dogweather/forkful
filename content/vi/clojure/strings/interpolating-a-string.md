---
title:                "Nội suy chuỗi ký tự"
aliases:
- /vi/clojure/interpolating-a-string/
date:                  2024-01-28T22:01:57.934346-07:00
model:                 gpt-4-0125-preview
simple_title:         "Nội suy chuỗi ký tự"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/clojure/interpolating-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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
