---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:55.346954-07:00
description: "So s\xE1nh hai ng\xE0y ngh\u0129a l\xE0 ki\u1EC3m tra ch\xFAng c\xF3\
  \ m\u1ED1i quan h\u1EC7 nh\u01B0 th\u1EBF n\xE0o\u2014m\u1ED9t c\xE1i c\xF3 tr\u01B0\
  \u1EDBc, sau, hay ch\xEDnh x\xE1c nh\u01B0 nhau so v\u1EDBi c\xE1i kia? C\xE1c l\u1EAD\
  p tr\xECnh vi\xEAn th\u1EF1c\u2026"
lastmod: '2024-02-25T18:49:34.546716-07:00'
model: gpt-4-0125-preview
summary: "So s\xE1nh hai ng\xE0y ngh\u0129a l\xE0 ki\u1EC3m tra ch\xFAng c\xF3 m\u1ED1\
  i quan h\u1EC7 nh\u01B0 th\u1EBF n\xE0o\u2014m\u1ED9t c\xE1i c\xF3 tr\u01B0\u1EDB\
  c, sau, hay ch\xEDnh x\xE1c nh\u01B0 nhau so v\u1EDBi c\xE1i kia? C\xE1c l\u1EAD\
  p tr\xECnh vi\xEAn th\u1EF1c\u2026"
title: "So s\xE1nh hai ng\xE0y"
---

{{< edit_this_page >}}

## Gì & Tại sao?
So sánh hai ngày nghĩa là kiểm tra chúng có mối quan hệ như thế nào—một cái có trước, sau, hay chính xác như nhau so với cái kia? Các lập trình viên thực hiện điều này để xử lý hạn chót, lên lịch sự kiện, và theo dõi dữ liệu liên quan đến thời gian.

## Làm thế nào:
Clojure sử dụng khả năng tương tác Java để xử lý ngày tháng. Hãy trải tay áo lên và nhảy vào:

```clojure
;; Nhập lớp Date của Java
(import java.util.Date)

;; Tạo hai thực thể ngày
(def date1 (java.util.Date.))
(Thread/sleep 1000) ;; Đợi một chút
(def date2 (java.util.Date.))

;; So sánh các ngày
(println (.before date1 date2)) ; true, date1 trước date2
(println (.after date1 date2))  ; false, date1 không sau date2
(println (.equals date1 date2)) ; false, date1 không giống như date2
```

Đầu ra mẫu có thể trông như thế này, nhưng với các dấu thời gian khác nhau:

```
true
false
false
```

## Đi sâu vào
Trong quá khứ, các nhà phát triển Clojure thường sử dụng `Date` của Java cho các hoạt động ngày tháng, gọi phương thức bằng cách sử dụng toán tử dấu chấm như đã thấy trước đây. Các phương án thay thế bao gồm `clj-time`, một thư viện Clojure bao bọc Joda-Time.

Ví dụ sử dụng `clj-time` sẽ trông như thế này:

```clojure
;; Thêm clj-time vào phần phụ thuộc dự án của bạn
(require '[clj-time.core :as time])
(require '[clj-time.coerce :as coerce])

;; Tạo hai thực thể thời gian ngày tháng
(def date-time1 (time/now))
(Thread/sleep 1000) ;; Đợi một giây
(def date-time2 (time/now))

;; So sánh sử dụng các hàm của clj-time
(println (time/before? date-time1 date-time2)) ; true
(println (time/after? date-time1 date-time2))  ; false
(println (time/equal? date-time1 date-time2))  ; false
```

Quan điểm của Clojure về thời gian là tận dụng các thư viện của Java, trong khi clj-time tích hợp với Joda-Time cho trải nghiệm Clojure phong cách hơn.

Kể từ Java 8, gói `java.time`—lấy cảm hứng từ Joda-Time—là cách ưa thích để xử lý ngày và giờ trong Java và, thông qua tương tác, trong Clojure. Thiết kế được cải thiện và các khả năng bổ sung như các múi giờ làm cho `java.time` trở thành một lựa chọn mạnh mẽ.

## Xem thêm
- [Tương tác Java trong Clojure](https://clojure.org/reference/java_interop)
- [Kho lưu trữ GitHub của clj-time](https://github.com/clj-time/clj-time)
- [Hướng dẫn API ngày và giờ của Java](https://docs.oracle.com/javase/tutorial/datetime/)
