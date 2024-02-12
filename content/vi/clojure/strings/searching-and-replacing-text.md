---
title:                "Tìm kiếm và thay thế văn bản"
aliases:
- vi/clojure/searching-and-replacing-text.md
date:                  2024-01-28T22:07:34.968805-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tìm kiếm và thay thế văn bản"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/clojure/searching-and-replacing-text.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Việc tìm kiếm và thay thế văn bản cho phép bạn tìm các chuỗi cụ thể trong một khối văn bản và thay thế chúng bằng cái khác. Lập trình viên làm điều này cho các chỉnh sửa nhanh chóng, tái cấu trúc lớn, hoặc xử lý văn bản tự động. Đây là một kỹ thuật thao tác văn bản cơ bản, nhưng mạnh mẽ.

## Làm thế nào:

Trong Clojure, chúng ta sử dụng hàm `clojure.string/replace` để tìm kiếm và thay thế văn bản. Chúng ta hãy đi thẳng vào một số mã:

```clojure
(require '[clojure.string :as str])

;; Thay thế cơ bản
(str/replace "I like apples" "apples" "oranges")
;; => "I like oranges"

;; Sử dụng biểu thức chính quy để thay thế tất cả nguyên âm
(str/replace "Hello, World!" "[AEIOUaeiou]" "*")
;; => "H*ll*, W*rld!"

;; Thay thế với một hàm cho những thay đổi động
(str/replace "I have 2 apples and 5 bananas"
             #"\d+"
             (fn [match] (str (inc (Integer/parseInt match)))))
;; => "I have 3 apples and 6 bananas"
```

Đơn giản như vậy. Chạy nó, và bạn sẽ thấy sự biến đổi ngay tại REPL của bạn.

## Đào Sâu

Việc tìm kiếm và thay thế trong văn bản không phải là mới. Nó cổ xưa trong ngành công nghiệp máy tính. Chúng ta đã nhận được nó từ những trình biên tập đầu tiên như `sed` trong Unix. Chúng ta đã đi được một chặng đường dài kể từ đó.

Clojure, được xây dựng trên JVM, có nghĩa là bạn có sức mạnh của biểu thức chính quy Java ngay bên dưới. Về mặt hiệu suất, nó khá tiện lợi cho các script nhanh nhưng nhớ rằng, lạm dụng trong xử lý văn bản quy mô lớn có thể ảnh hưởng đến hiệu suất.

Về các lựa chọn khác, ngoài `clojure.string/replace`, có các thư viện dựa trên regex hay thậm chí là viết hàm tùy chỉnh của riêng bạn nếu bạn cảm thấy mạo hiểm. Hãy nghĩ về việc sử dụng `replace-first` nếu bạn chỉ cần một sự thay đổi đơn lầm.

Về mặt chức năng, cách tiếp cận về bất biến của Clojure có nghĩa là mỗi lần thay thế sẽ tạo ra một chuỗi mới. Không có chuỗi có thể thay đổi có nghĩa là ít lỗi và bất ngờ hơn.

## Xem Thêm

Để tìm hiểu sâu hơn, hãy xem các nguồn lực này:

- Tài liệu API `clojure.string` của Clojure [Tài liệu API](https://clojuredocs.org/clojure.string/replace)
- Về biểu thức chính quy, lớp `Pattern` của Java [Lớp Pattern](https://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html)
