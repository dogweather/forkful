---
title:                "Sắp xếp mã thành các hàm"
date:                  2024-01-28T22:03:10.656975-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sắp xếp mã thành các hàm"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/clojure/organizing-code-into-functions.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Chia code thành các hàm là về việc đóng gói các khối code để hoàn thành các nhiệm vụ cụ thể. Làm như vậy giúp code của bạn sạch sẽ, dễ bảo trì hơn và dễ đọc hơn cho các lập trình viên khác.

## Làm thế nào:

Hàm trong Clojure được định nghĩa với `defn`, theo sau là tên, tham số và phần thân. Dưới đây là một ví dụ nhanh.

```Clojure
(defn greet [name]
  (str "Hello, " name "!"))

(greet "Alex") ; => "Hello, Alex!"
```

Giờ hãy nói chúng ta muốn tính diện tích của một hình chữ nhật. Thay vì gộp tất cả lại với nhau, chúng ta tách nó ra làm hai hàm:

```Clojure
(defn area [length width]
  (* length width))

(defn print-area [length width]
  (println "Diện tích là:" (area length width)))

(print-area 3 4) ; => Diện tích là: 12
```

## Sâu hơn nữa

Từ xa xưa, các lập trình viên thường chỉ nhét hết logic của mình vào một khối duy nhất. Nó xấu xí. Sau đó, lập trình cấu trúc xuất hiện, và hàm trở thành một điều gì đó. Trong Clojure, mọi hàm đều là hạng nhất—bạn có thể sử dụng chúng như bất kỳ giá trị nào khác.

Các phương án khác? Một số người có thể lộn xộn với các phương thức đa-hình hoặc hàm bậc cao, nhưng những thứ đó chỉ là gia vị trong món hầm hàm.

Tất cả trong chi tiết của một hàm: chúng không thể thay đổi trong Clojure, giúp giảm thiểu khả năng xáo trộn do tác dụng phụ. Chúng nghiêng về việc sử dụng đệ quy thay vì các vòng lặp thông thường, điều này phù hợp với các nguyên tắc lập trình hàm của ngôn ngữ.

## Xem thêm

- Hướng dẫn của chính Clojure: https://clojure.org/guides/learn/functions
- Cơ bản về Lập trình Hàm: https://www.braveclojure.com/core-functions-in-depth/
- Các buổi nói chuyện của Rich Hickey: https://changelog.com/posts/rich-hickeys-greatest-hits - để hiểu sâu hơn về triết lý của Clojure.
