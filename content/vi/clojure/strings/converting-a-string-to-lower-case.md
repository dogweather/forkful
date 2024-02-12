---
title:                "Chuyển đổi chuỗi thành chữ thường"
aliases:
- /vi/clojure/converting-a-string-to-lower-case/
date:                  2024-01-28T21:57:57.060537-07:00
model:                 gpt-4-0125-preview
simple_title:         "Chuyển đổi chuỗi thành chữ thường"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/clojure/converting-a-string-to-lower-case.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?
Chuyển một chuỗi văn bản thành chữ thường có nghĩa là biến đổi tất cả các ký tự trong văn bản thành các biến thể chữ thường của chúng, như chuyển "Hello, World!" thành "hello, world!". Lập trình viên làm điều này để đảm bảo tính nhất quán, đặc biệt trong các nhiệm vụ như so sánh đầu vào của người dùng, nơi mà việc phân biệt chữ hoa và chữ thường không nên quan trọng.

## Cách thực hiện:
Trong Clojure, để chuyển một chuỗi văn bản thành chữ thường, bạn sẽ sử dụng hàm `clojure.string/lower-case`. Hãy xem nó đơn giản như thế nào:

```clojure
(require '[clojure.string :as str])

(str/lower-case "Hello, World!") ; => "hello, world!"
```

Kết quả rất dễ hiểu:

```clojure
"hello, world!"
```

## Sâu hơn nữa
Trong lịch sử, việc chuyển đổi chữ hoa thành chữ thường đã tồn tại từ những ngày đầu của việc tính toán để hài hòa hóa việc xử lý dữ liệu văn bản. Trong Clojure, hàm `clojure.string/lower-case` là một phần của thư viện `clojure.string`, một tập hợp các công cụ hỗ trợ cho việc thao tác với chuỗi, được bao gồm trong ngôn ngữ cốt lõi.

Các lựa chọn khác cho `clojure.string/lower-case` bao gồm việc tạo ra chính hàm của bạn thông qua việc ánh xạ với việc điều khiển `char`, nhưng đó là việc tái phát minh bánh xe khi bạn có một hàm tích hợp sẵn được tối ưu hóa và đã được kiểm nghiệm kỹ lưỡng.

Về nội bộ, `clojure.string/lower-case` chuyển gánh nặng công việc cho phương thức `toLowerCase` của Java, vì Clojure chạy trên Java Virtual Machine (JVM). Điều này đảm bảo hiệu suất cao vì nó tận dụng các thư viện chín muồi của Java.

## Xem thêm
- API `clojure.string` của Clojure: https://clojuredocs.org/clojure.string
- Phương thức `String.toLowerCase()` của Java: https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#toLowerCase()
