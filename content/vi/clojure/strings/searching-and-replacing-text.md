---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:34.968805-07:00
description: "Vi\u1EC7c t\xECm ki\u1EBFm v\xE0 thay th\u1EBF v\u0103n b\u1EA3n cho\
  \ ph\xE9p b\u1EA1n t\xECm c\xE1c chu\u1ED7i c\u1EE5 th\u1EC3 trong m\u1ED9t kh\u1ED1\
  i v\u0103n b\u1EA3n v\xE0 thay th\u1EBF ch\xFAng b\u1EB1ng c\xE1i kh\xE1c. L\u1EAD\
  p tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y\u2026"
lastmod: '2024-03-11T00:14:09.378601-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c t\xECm ki\u1EBFm v\xE0 thay th\u1EBF v\u0103n b\u1EA3n cho ph\xE9\
  p b\u1EA1n t\xECm c\xE1c chu\u1ED7i c\u1EE5 th\u1EC3 trong m\u1ED9t kh\u1ED1i v\u0103\
  n b\u1EA3n v\xE0 thay th\u1EBF ch\xFAng b\u1EB1ng c\xE1i kh\xE1c. L\u1EADp tr\xEC\
  nh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y\u2026"
title: "T\xECm ki\u1EBFm v\xE0 thay th\u1EBF v\u0103n b\u1EA3n"
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
