---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:07.466916-07:00
description: "Ph\xE2n t\xEDch ng\xE0y t\u1EEB m\u1ED9t chu\u1ED7i ngh\u0129a l\xE0\
  \ chuy\u1EC3n \u0111\u1ED5i v\u0103n b\u1EA3n ng\xE0y d\u1EC5 \u0111\u1ECDc c\u1EE7\
  a con ng\u01B0\u1EDDi sang \u0111\u1ECBnh d\u1EA1ng m\xE0 m\xE1y t\xEDnh c\xF3 th\u1EC3\
  \ hi\u1EC3u \u0111\u01B0\u1EE3c. L\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n\u2026"
lastmod: '2024-03-13T22:44:36.165606-06:00'
model: gpt-4-0125-preview
summary: "Ph\xE2n t\xEDch ng\xE0y t\u1EEB m\u1ED9t chu\u1ED7i ngh\u0129a l\xE0 chuy\u1EC3\
  n \u0111\u1ED5i v\u0103n b\u1EA3n ng\xE0y d\u1EC5 \u0111\u1ECDc c\u1EE7a con ng\u01B0\
  \u1EDDi sang \u0111\u1ECBnh d\u1EA1ng m\xE0 m\xE1y t\xEDnh c\xF3 th\u1EC3 hi\u1EC3\
  u \u0111\u01B0\u1EE3c."
title: "Ph\xE2n t\xEDch ng\xE0y t\u1EEB chu\u1ED7i k\xED t\u1EF1"
weight: 30
---

## Cách thực hiện:
Clojure dựa vào Java để phân tích ngày, vì vậy chúng ta sẽ sử dụng `java.time.LocalDate` ở đây:

```Clojure
(require '[java-time :as jt])

(defn parse-date [date-str]
  (jt/local-date "yyyy-MM-dd" date-str))

(println (parse-date "2023-04-05"))
```

Đầu ra:

```
#object[java.time.LocalDate 0x4b121a5e "2023-04-05"]
```

Ở đây `java-time` là một thư viện Clojure bao bọc APIs `java.time`. Nó mang tính cách Clojure hơn là sử dụng trực tiếp Java interop.

## Xem Xét Kỹ Lưỡng
Clojure, ra đời năm 2007, là một Lisp hiện đại chạy trên JVM. Nó cung cấp khả năng tương tác với Java, bao gồm cả xử lý ngày. Trước `java.time` (được giới thiệu trong Java 8), Java sử dụng `java.util.Date` và `java.text.SimpleDateFormat`, cả hai đều kém linh hoạt và ít an toàn về luồng hơn.

`clj-time`, một bộ bao của Joda-Time, từng phổ biến với Clojure trước khi có `java-time`, nhưng Joda-Time giờ được coi là lỗi thời. Ngày nay, `java-time` là lựa chọn ưu tiên vì nó bao quanh gói `java.time`, nổi bật hơn hẳn và mặc định là bất biến.

Cũng có các thư viện Clojure thuần túy, như `tick`, nhưng chúng cũng xây dựng dựa trên `java.time` của Java vì lý do thiết thực. Gói `java.time` cơ bản sử dụng hệ thống lịch ISO nhưng cũng hỗ trợ những hệ thống khác. Sự linh hoạt đó có nghĩa là các chương trình Clojure không chỉ thân thiện với JVM mà còn sẵn sàng cho quốc tế.

## Xem Thêm
- [Clojure Docs](https://clojure.org/)
- [Thư viện java-time](https://github.com/dm3/clojure.java-time)
- [Thư viện clj-time cũ](https://github.com/clj-time/clj-time)
- [Java SE Date Time](https://docs.oracle.com/javase/tutorial/datetime/)

Hãy tiếp tục khám phá và chúc bạn lập trình vui vẻ!
