---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:55:43.770400-07:00
description: "Vi\u1EC7c t\xEDnh to\xE1n ng\xE0y trong t\u01B0\u01A1ng lai ho\u1EB7\
  c qu\xE1 kh\u1EE9 bao g\u1ED3m vi\u1EC7c thao t\xE1c v\u1EDBi ng\xE0y \u0111\u1EC3\
  \ t\xECm ra ch\xFAng s\u1EBD tr\u1EDF n\xEAn nh\u01B0 th\u1EBF n\xE0o sau m\u1ED9\
  t kho\u1EA3ng th\u1EDDi gian nh\u1EA5t \u0111\u1ECBnh\u2026"
lastmod: '2024-03-13T22:44:36.170649-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c t\xEDnh to\xE1n ng\xE0y trong t\u01B0\u01A1ng lai ho\u1EB7c qu\xE1\
  \ kh\u1EE9 bao g\u1ED3m vi\u1EC7c thao t\xE1c v\u1EDBi ng\xE0y \u0111\u1EC3 t\xEC\
  m ra ch\xFAng s\u1EBD tr\u1EDF n\xEAn nh\u01B0 th\u1EBF n\xE0o sau m\u1ED9t kho\u1EA3\
  ng th\u1EDDi gian nh\u1EA5t \u0111\u1ECBnh\u2026"
title: "T\xEDnh to\xE1n ng\xE0y trong t\u01B0\u01A1ng lai ho\u1EB7c qu\xE1 kh\u1EE9"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Việc tính toán ngày trong tương lai hoặc quá khứ bao gồm việc thao tác với ngày để tìm ra chúng sẽ trở nên như thế nào sau một khoảng thời gian nhất định hoặc chúng đã là như thế nào. Lập trình viên thực hiện điều này cho các mục đích như lên lịch sự kiện, nhắc nhở, hoặc xác định ngày hết hạn.

## Cách làm:

Trong Clojure, bạn chủ yếu sử dụng thư viện `clj-time` cho các thao tác với ngày. Dưới đây là một ví dụ nhanh:

```clojure
(require '[clj-time.core :as time])
(require '[clj-time.coerce :as coerce])
(require '[clj-time.periodic :as periodic])

;; Thêm 5 ngày vào ngày hiện tại
(let [now (time/now)
      five-days (time/plus now (time/days 5))]
  (str "Năm ngày kể từ bây giờ: " (coerce/to-string five-days)))

;; Trừ 10 ngày từ một ngày cụ thể
(let [specific-date (coerce/to-date-time "2023-03-01T12:00:00.000Z")
      ten-days-ago (time/minus specific-date (time/days 10))]
  (str "Mười ngày trước Ngày 1 tháng 3 năm 2023: " (coerce/to-string ten-days-ago)))
```

Kết quả mẫu:
```
"Năm ngày kể từ bây giờ: 2023-03-23T08:00:00.000Z"
"Mười ngày trước Ngày 1 tháng 3 năm 2023: 2023-02-19T12:00:00.000Z"
```

## Tìm hiểu sâu

Trong những ngày đầu, các lập trình viên sử dụng các lớp `Date` và `Calendar` của Java. Nhưng, phải thừa nhận, chúng khá là phiền phức—dài dòng và dễ mắc lỗi. Thư viện `clj-time` đã mang lại sự thoải mái, bao bọc API thân thiện hơn với nhà phát triển của Joda-Time.

Có lựa chọn nào khác không? Java 8 đã giới thiệu `java.time` (JSR-310), được đánh giá khá tốt, nhưng trong lĩnh vực của Clojure, chúng ta vẫn còn ấm áp với `clj-time`.

Khi tính toán ngày, bạn sử dụng các khoảng thời gian cho các khái niệm như "ngày" và "tháng" và thời lượng cho số lượng mili giây chính xác. Hãy lưu ý về múi giờ—ngày và giờ có thể thay đổi đáng kể tùy thuộc vào luật múi giờ, và giờ tiết kiệm ánh sáng ban ngày (DST) có thể làm gián đoạn công việc của bạn.

## Xem thêm

- Kho GitHub của `clj-time`: [https://github.com/clj-time/clj-time](https://github.com/clj-time/clj-time)
- `java-time` của Clojure: [https://github.com/dm3/clojure.java-time](https://github.com/dm3/clojure.java-time)
