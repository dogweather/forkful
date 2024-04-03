---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:10.656975-07:00
description: "L\xE0m th\u1EBF n\xE0o: H\xE0m trong Clojure \u0111\u01B0\u1EE3c \u0111\
  \u1ECBnh ngh\u0129a v\u1EDBi `defn`, theo sau l\xE0 t\xEAn, tham s\u1ED1 v\xE0 ph\u1EA7\
  n th\xE2n. D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9t v\xED d\u1EE5 nhanh."
lastmod: '2024-03-13T22:44:36.160484-06:00'
model: gpt-4-0125-preview
summary: "H\xE0m trong Clojure \u0111\u01B0\u1EE3c \u0111\u1ECBnh ngh\u0129a v\u1EDB\
  i `defn`, theo sau l\xE0 t\xEAn, tham s\u1ED1 v\xE0 ph\u1EA7n th\xE2n."
title: "S\u1EAFp x\u1EBFp m\xE3 th\xE0nh c\xE1c h\xE0m"
weight: 18
---

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
