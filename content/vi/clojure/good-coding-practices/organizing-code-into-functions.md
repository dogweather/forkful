---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:10.656975-07:00
description: "Chia code th\xE0nh c\xE1c h\xE0m l\xE0 v\u1EC1 vi\u1EC7c \u0111\xF3\
  ng g\xF3i c\xE1c kh\u1ED1i code \u0111\u1EC3 ho\xE0n th\xE0nh c\xE1c nhi\u1EC7m\
  \ v\u1EE5 c\u1EE5 th\u1EC3. L\xE0m nh\u01B0 v\u1EADy gi\xFAp code c\u1EE7a b\u1EA1\
  n s\u1EA1ch s\u1EBD, d\u1EC5 b\u1EA3o tr\xEC h\u01A1n v\xE0 d\u1EC5\u2026"
lastmod: '2024-03-13T22:44:36.160484-06:00'
model: gpt-4-0125-preview
summary: "Chia code th\xE0nh c\xE1c h\xE0m l\xE0 v\u1EC1 vi\u1EC7c \u0111\xF3ng g\xF3\
  i c\xE1c kh\u1ED1i code \u0111\u1EC3 ho\xE0n th\xE0nh c\xE1c nhi\u1EC7m v\u1EE5\
  \ c\u1EE5 th\u1EC3. L\xE0m nh\u01B0 v\u1EADy gi\xFAp code c\u1EE7a b\u1EA1n s\u1EA1\
  ch s\u1EBD, d\u1EC5 b\u1EA3o tr\xEC h\u01A1n v\xE0 d\u1EC5\u2026"
title: "S\u1EAFp x\u1EBFp m\xE3 th\xE0nh c\xE1c h\xE0m"
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
