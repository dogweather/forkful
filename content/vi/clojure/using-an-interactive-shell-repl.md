---
title:                "Sử dụng vỏ tương tác (REPL)"
date:                  2024-01-28T22:09:33.866291-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng vỏ tương tác (REPL)"

category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/clojure/using-an-interactive-shell-repl.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Là gì & Tại sao?
REPL, hay Vòng lặp Đọc-Đánh giá-In, là một môi trường lập trình cho phép thử nghiệm động từng đoạn mã Clojure một cách đơn lẻ. Lập trình viên sử dụng nó để nhận phản hồi ngay lập tức, phát triển tuần tự và thử nghiệm nhanh chóng mà không cần đến quá trình biên dịch hay thiết lập một môi trường dự án hoàn chỉnh.

## Cách thực hiện:
Bắt đầu bằng cách khởi chạy REPL:

```Clojure
user=> (println "Xin chào, REPL!")
Xin chào, REPL!
nil
```

Định nghĩa một hàm và thử nó:
```Clojure
user=> (defn greet [name] (str "Xin chào, " name "!"))
#'user/greet
user=> (greet "Người lập trình Clojure")
"Xin chào, Người lập trình Clojure!"
```

Thử nghiệm với các cấu trúc dữ liệu:
```Clojure
user=> (def my-map {:a 1 :b 2})
#'user/my-map
user=> (assoc my-map :c 3)
{:a 1, :b 2, :c 3}
```

## Sâu hơn
REPL là chìa khóa cho triết lý phát triển tương tác của gia đình Lisp, và Clojure, một phương ngữ Lisp hiện đại, đã tận dụng tuyệt vời công cụ này. Nó có nguồn gốc từ REPL Lisp đầu tiên vào cuối những năm 1950. Các lựa chọn thay thế trong các ngôn ngữ khác bao gồm bộ thông dịch Python và bảng điều khiển Node.js, nhưng REPL của Clojure có vị thế quan trọng và là một phần không thể thiếu trong quy trình làm việc.

Một phiên làm việc REPL của Clojure có thể được tích hợp vào các môi trường khác nhau như dòng lệnh, các môi trường phát triển tích hợp (IDEs) (như IntelliJ với Cursive, hoặc Emacs với CIDER), hoặc các công cụ dựa trên trình duyệt như Nightcode. Một cách sâu sắc hơn, REPL trao quyền cho nhà phát triển để thao tác với các cấu trúc ngôn ngữ tại thời điểm chạy và duy trì các trạng thái qua các biến đổi khác nhau, thường dẫn đến lập trình khám phá và mã nguồn mạnh mẽ hơn.

Chức năng của REPL tỏa sáng với các công cụ như `lein repl` hoặc `clj`, cho phép quản lý phụ thuộc, các plugin khác nhau, và các tùy chỉnh cụ thể cho dự án, dẫn đến một quy trình phát triển linh hoạt và năng suất hơn.

## Xem thêm
- Hướng dẫn trên trang web chính thức của Clojure về REPL: https://clojure.org/guides/repl/introduction
- Bài nói của Rich Hickey về phát triển dựa trên REPL: https://www.youtube.com/watch?v=Qx0-pViyIDU
- Clojure thực hành: sử dụng REPL cho phát triển tuần tự: http://practicalclj.blogspot.com/2009/10/using-clojure-repl.html
