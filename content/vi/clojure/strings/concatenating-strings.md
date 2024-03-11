---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:07.987596-07:00
description: "N\u1ED1i chu\u1ED7i c\xF3 ngh\u0129a l\xE0 gh\xE9p ch\xFAng l\u1EA1\
  i v\u1EDBi nhau - \"hello\" + \"world\" tr\u1EDF th\xE0nh \"helloworld\". L\u1EAD\
  p tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 x\xE2y d\u1EF1ng v\u0103\
  n b\u1EA3n, nh\u01B0 l\xE0 URL,\u2026"
lastmod: '2024-03-11T00:14:09.388060-06:00'
model: gpt-4-0125-preview
summary: "N\u1ED1i chu\u1ED7i c\xF3 ngh\u0129a l\xE0 gh\xE9p ch\xFAng l\u1EA1i v\u1EDB\
  i nhau - \"hello\" + \"world\" tr\u1EDF th\xE0nh \"helloworld\". L\u1EADp tr\xEC\
  nh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 x\xE2y d\u1EF1ng v\u0103n b\u1EA3\
  n, nh\u01B0 l\xE0 URL,\u2026"
title: "N\u1ED1i chu\u1ED7i k\xFD t\u1EF1"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Nối chuỗi có nghĩa là ghép chúng lại với nhau - "hello" + "world" trở thành "helloworld". Lập trình viên làm điều này để xây dựng văn bản, như là URL, thông điệp, hoặc kết quả dựa trên nhập liệu của người dùng hoặc dữ liệu chương trình.

## Làm thế nào:

Clojure làm cho việc nối chuỗi trở nên dễ dàng với hàm `str`. Hãy cùng khám phá ngay:

```clojure
;; Nối chuỗi đơn giản với hàm str
(str "Hello, " "world!")
;; => "Hello, world!"

;; Nối nhiều chuỗi
(str "Clojure" " is" " awesome!")
;; => "Clojure is awesome!"

;; Kết hợp chuỗi và các giá trị khác
(str "Câu trả lời là " 42)
;; => "Câu trả lời là 42"

;; Sử dụng apply để nối một chuỗi các chuỗi
(apply str ["Ghép" " " "những" " " "chuỗi này!"])
;; => "Ghép những chuỗi này!"
```

Tuyệt, vậy là bạn đã thấy nó hoạt động. Chỉ cần nhớ rằng hàm `str` làm việc với bất kỳ giá trị nào bằng cách gọi `toString` trên nó. Nếu nó là nil, bạn sẽ nhận được chuỗi "nil".

## Khám phá Sâu hơn

Lịch sử, việc nối chuỗi đã tồn tại kể từ khi chúng ta cần xử lý văn bản một cách lập trình, và mỗi ngôn ngữ đều cung cấp những phương pháp của riêng mình. Trong Clojure, `str` là một phần của thư viện cốt lõi, được giới thiệu vì sự đơn giản và tính đồng nhất.

Có phương pháp thay thế cho `str` không? Có! `StringBuilder` có thể hiệu quả hơn đối với việc nối nhiều chuỗi, đặc biệt trong các vòng lặp. Clojure có thể gọi các phương thức Java, vì vậy bạn cũng có thể sử dụng `StringBuilder` như sau:

```clojure
;; Sử dụng StringBuilder cho hiệu quả
(let [builder (StringBuilder.)]
  (.append builder "Đây là")
  (.append builder " cách thức")
  (.append builder " hiệu quả hơn!")
  (.toString builder))
;; => "Đây là cách thức hiệu quả hơn!"
```

Tại sao không luôn sử dụng `StringBuilder`? Đối với hầu hết các tác vụ hàng ngày, `str` đơn giản và đủ nhanh. `StringBuilder` tỏa sáng trong các kịch bản cần hiệu suất cao với nhiều nối chuỗi.

Về mặt triển khai, vì Clojure được hosting trên JVM, nó hưởng lợi từ khả năng xử lý chuỗi của Java. Tuy nhiên, giống như trong các `String` của Java, mỗi lần gọi `str` tạo ra một `String` mới, điều này có thể là một yếu tố cần cân nhắc về bộ nhớ.

## Xem Thêm

- Tài liệu hàm `str` của Clojure: [Clojure Strings](https://clojuredocs.org/clojure.core/str)
- `StringBuilder` của Java: [Tài liệu StringBuilder](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/StringBuilder.html)
- Hướng dẫn Clojure thực tế về `str` và nhiều hơn: [Clojure for the Brave and True](https://www.braveclojure.com/clojure-for-the-brave-and-true/)
