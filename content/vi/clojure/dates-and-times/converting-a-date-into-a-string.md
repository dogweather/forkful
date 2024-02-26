---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:50.692502-07:00
description: "Chuy\u1EC3n \u0111\u1ED5i m\u1ED9t ng\xE0y th\xE0nh chu\u1ED7i ngh\u0129\
  a l\xE0 bi\u1EBFn m\u1ED9t \u0111\u1ED1i t\u01B0\u1EE3ng ng\xE0y th\xE1ng th\xE0\
  nh v\u0103n b\u1EA3n d\u1EC5 \u0111\u1ECDc cho con ng\u01B0\u1EDDi. L\u1EADp tr\xEC\
  nh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 hi\u1EC3n th\u1ECB c\xE1c\u2026"
lastmod: '2024-02-25T18:49:34.545464-07:00'
model: gpt-4-0125-preview
summary: "Chuy\u1EC3n \u0111\u1ED5i m\u1ED9t ng\xE0y th\xE0nh chu\u1ED7i ngh\u0129\
  a l\xE0 bi\u1EBFn m\u1ED9t \u0111\u1ED1i t\u01B0\u1EE3ng ng\xE0y th\xE1ng th\xE0\
  nh v\u0103n b\u1EA3n d\u1EC5 \u0111\u1ECDc cho con ng\u01B0\u1EDDi. L\u1EADp tr\xEC\
  nh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 hi\u1EC3n th\u1ECB c\xE1c\u2026"
title: "Chuy\u1EC3n \u0111\u1ED5i m\u1ED9t ng\xE0y th\xE0nh chu\u1ED7i"
---

{{< edit_this_page >}}

## Điều gì & Tại sao?
Chuyển đổi một ngày thành chuỗi nghĩa là biến một đối tượng ngày tháng thành văn bản dễ đọc cho con người. Lập trình viên làm điều này để hiển thị các ngày tháng theo các định dạng dễ hiểu hoặc để tuần tự hóa chúng cho việc lưu trữ và truyền tải.

## Cách thực hiện:
Trong Clojure, chúng ta sử dụng các khả năng tương tác với Java để định dạng ngày tháng. Dưới đây là hướng dẫn nhanh:

```clojure
(import java.text.SimpleDateFormat)
(import java.util.Date)

;; Tạo một đối tượng ngày (hãy sử dụng ngày và giờ hiện tại)
(def now (Date.))

;; Thiết lập định dạng mong muốn
(def formatter (SimpleDateFormat. "yyyy-MM-dd HH:mm:ss"))

;; Định dạng ngày thành chuỗi
(def formatted-date (.format formatter now))

;; In nó ra
(println formatted-date)
;; Đầu ra có thể là: "2023-03-15 09:26:45" (tùy thuộc vào ngày và giờ hiện tại)
```

## Tìm hiểu sâu
Việc chuyển đổi ngày tháng thành chuỗi không chỉ riêng có trong Clojure; đó là một thao tác phổ biến trong nhiều ngôn ngữ lập trình. Nhu cầu này phát sinh từ khi máy tính bắt đầu xử lý ngày tháng vì việc biểu diễn theo cách dễ hiểu cho con người làm dễ dàng hơn cho việc hiểu và giao tiếp, trong khi máy móc thích các định dạng dữ liệu có cấu trúc hơn.

Trong Clojure, do chạy trên Máy ảo Java (JVM), chúng ta thường tận dụng các thư viện ngày giờ của Java, như `java.util.Date` và `java.text.SimpleDateFormat`. Mặc dù các lớp này đã tồn tại từ lâu, gói `java.time` mới hơn (được giới thiệu trong Java 8) là một lựa chọn thay thế với độ an toàn về luồng và API trực quan hơn.

Clojure không có thư viện định dạng ngày được tích hợp sẵn là một phần của ngôn ngữ cốt lõi, do đó thường sử dụng tương tác Java hoặc thư viện bên thứ ba, như `clj-time` (một bộ bọc quanh Joda Time) để có giải pháp Clojure đậm chất ngôn ngữ hơn.

Dưới đây là cách bạn có thể sử dụng `java.time` để định dạng:

```clojure
(import java.time.LocalDateTime)
(import java.time.format.DateTimeFormatter)

;; Tạo một đối tượng ngày (ngày và giờ hiện tại)
(def now (LocalDateTime/now))

;; Thiết lập định dạng mong muốn
(def formatter (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss"))

;; Định dạng ngày thành chuỗi
(def formatted-date (.format now formatter))

;; In nó ra
(println formatted-date)
;; Đầu ra tương tự như trước, với ngày và giờ hiện tại
```

Phương pháp này tránh được các vấn đề về tính biến đổi của SimpleDateFormat và nên được ưu tiên trong mã mới khi mối quan tâm là độ an toàn về luồng.

## Xem thêm
- Hướng dẫn Ngày và Giờ của Java 8: [https://docs.oracle.com/javase/tutorial/datetime/](https://docs.oracle.com/javase/tutorial/datetime/)
- ClojureDocs, một kho lưu trữ tài liệu và ví dụ do cộng đồng hỗ trợ: [https://clojuredocs.org/](https://clojuredocs.org/)
- clj-time, một thư viện ngày và giờ cho Clojure: [https://github.com/clj-time/clj-time](https://github.com/clj-time/clj-time)
