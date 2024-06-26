---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:57.944134-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Clojure l\xE0m cho vi\u1EC7c l\xE0m vi\u1EC7\
  c v\u1EDBi chu\u1ED7i tr\u1EDF n\xEAn d\u1EC5 d\xE0ng. \u0110\u1EC3 r\xFAt tr\xED\
  ch chu\u1ED7i con, `subs` l\xE0 h\xE0m b\u1EA1n c\u1EA7n \u0111\u1EBFn."
lastmod: '2024-03-13T22:44:36.138329-06:00'
model: gpt-4-0125-preview
summary: "Clojure l\xE0m cho vi\u1EC7c l\xE0m vi\u1EC7c v\u1EDBi chu\u1ED7i tr\u1EDF\
  \ n\xEAn d\u1EC5 d\xE0ng."
title: "Tr\xEDch xu\u1EA5t chu\u1ED7i con"
weight: 6
---

## Cách thực hiện:
Clojure làm cho việc làm việc với chuỗi trở nên dễ dàng. Để rút trích chuỗi con, `subs` là hàm bạn cần đến:

```clojure
(let [text "ClojureRocks"]
  (subs text 7)) ; => "Rocks"

(let [text "ClojureRocks"]
  (subs text 0 7)) ; => "Clojure"
```

Và đó là tất cả—cung cấp chỉ số bắt đầu, và tùy chọn một chỉ số kết thúc, và bạn sẽ cắt chuỗi đúng như bạn cần.

## Sâu hơn nữa
Việc rút trích chuỗi con không phải là mới—đã tồn tại từ những ngày đầu của lập trình. Trong Clojure, `subs` là một hàm đơn giản. Nó là một phần của khả năng tương tác Java của Clojure, lợi dụng phương thức `substring` của Java. Hai điểm chính: không cho phép chỉ số âm, và nó bắt đầu từ số không (bắt đầu đếm từ số không). Vì vậy, hãy nhớ điều đó hoặc bạn sẽ lỡ mất một.

Có lựa chọn khác? Chắc chắn. Regex với `re-find` và `re-matcher` cho các mẫu phức tạp, hoặc `split` nếu bạn đang chia tại một dấu phân cách. Mỗi công cụ có vị trí của nó, nhưng không có gì đánh bại `subs` về sự đơn giản.

Về mặt thực thi, `subs` không sao chép ký tự, nó chia sẻ mảng ký tự gốc của chuỗi. Hiệu quả, nhưng nếu chuỗi gốc của bạn rất lớn và tất cả những gì bạn cần chỉ là một chút nhỏ, bạn có thể vô tình giữ toàn bộ chuỗi lớn trong bộ nhớ.

## Xem thêm:
- API Chuỗi Clojure chính thức: [clojure.string](https://clojuredocs.org/clojure.string)
- `substring` Java: Bởi vì đó là nguồn lực chính đằng sau `subs`. [Java substring](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#substring(int,%20int))
- Biểu thức chính quy trong Clojure: [re-find](https://clojuredocs.org/clojure.core/re-find)
- Chia chuỗi trong Clojure: [split](https://clojuredocs.org/clojure.string/split)
