---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:57.060537-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Trong Clojure, \u0111\u1EC3 chuy\u1EC3\
  n m\u1ED9t chu\u1ED7i v\u0103n b\u1EA3n th\xE0nh ch\u1EEF th\u01B0\u1EDDng, b\u1EA1\
  n s\u1EBD s\u1EED d\u1EE5ng h\xE0m `clojure.string/lower-case`. H\xE3y xem n\xF3\
  \ \u0111\u01A1n gi\u1EA3n nh\u01B0 th\u1EBF\u2026"
lastmod: '2024-03-13T22:44:36.135476-06:00'
model: gpt-4-0125-preview
summary: "Trong Clojure, \u0111\u1EC3 chuy\u1EC3n m\u1ED9t chu\u1ED7i v\u0103n b\u1EA3\
  n th\xE0nh ch\u1EEF th\u01B0\u1EDDng, b\u1EA1n s\u1EBD s\u1EED d\u1EE5ng h\xE0m\
  \ `clojure.string/lower-case`."
title: "Chuy\u1EC3n \u0111\u1ED5i chu\u1ED7i th\xE0nh ch\u1EEF th\u01B0\u1EDDng"
weight: 4
---

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
