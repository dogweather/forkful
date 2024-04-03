---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:07.987596-07:00
description: "L\xE0m th\u1EBF n\xE0o: Clojure l\xE0m cho vi\u1EC7c n\u1ED1i chu\u1ED7\
  i tr\u1EDF n\xEAn d\u1EC5 d\xE0ng v\u1EDBi h\xE0m `str`. H\xE3y c\xF9ng kh\xE1m\
  \ ph\xE1 ngay."
lastmod: '2024-03-13T22:44:36.142133-06:00'
model: gpt-4-0125-preview
summary: "Clojure l\xE0m cho vi\u1EC7c n\u1ED1i chu\u1ED7i tr\u1EDF n\xEAn d\u1EC5\
  \ d\xE0ng v\u1EDBi h\xE0m `str`."
title: "N\u1ED1i chu\u1ED7i k\xFD t\u1EF1"
weight: 3
---

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
