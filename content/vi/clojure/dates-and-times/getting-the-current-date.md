---
aliases:
- /vi/clojure/getting-the-current-date/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:44.530543-07:00
description: "L\u1EA5y ng\xE0y hi\u1EC7n t\u1EA1i trong Clojure c\xF3 ngh\u0129a l\xE0\
  \ l\u1EA5y ng\xE0y l\u1ECBch hi\u1EC7n t\u1EA1i m\xE0 ch\u01B0\u01A1ng tr\xECnh\
  \ c\u1EE7a b\u1EA1n \u0111ang ch\u1EA1y. L\u1EADp tr\xECnh vi\xEAn l\u1EA5y ng\xE0\
  y \u0111\u1EC3 \u0111\xF3ng d\u1EA5u th\u1EDDi gian cho\u2026"
lastmod: 2024-02-18 23:08:50.326295
model: gpt-4-0125-preview
summary: "L\u1EA5y ng\xE0y hi\u1EC7n t\u1EA1i trong Clojure c\xF3 ngh\u0129a l\xE0\
  \ l\u1EA5y ng\xE0y l\u1ECBch hi\u1EC7n t\u1EA1i m\xE0 ch\u01B0\u01A1ng tr\xECnh\
  \ c\u1EE7a b\u1EA1n \u0111ang ch\u1EA1y. L\u1EADp tr\xECnh vi\xEAn l\u1EA5y ng\xE0\
  y \u0111\u1EC3 \u0111\xF3ng d\u1EA5u th\u1EDDi gian cho\u2026"
title: "L\u1EA5y ng\xE0y hi\u1EC7n t\u1EA1i"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Lấy ngày hiện tại trong Clojure có nghĩa là lấy ngày lịch hiện tại mà chương trình của bạn đang chạy. Lập trình viên lấy ngày để đóng dấu thời gian cho các sự kiện, hết hạn cache, hoặc cho bất kỳ tính năng nhạy cảm với thời gian nào.

## Cách thực hiện:

```Clojure
;; Nhập Java interop để sử dụng các lớp Date
(import java.util.Date)
(import java.text.SimpleDateFormat)

;; Lấy Ngày và Giờ Hiện Tại
(defn current-date-time []
  (let [today (new Date)]
    (println "Ngày và giờ hiện tại: " today)))

(current-date-time)
;; Đầu ra: Ngày và giờ hiện tại:  Wed Apr 05 00:12:35 BST 2023

;; Định dạng Ngày theo Một Mẫu Cụ Thể
(defn formatted-current-date []
  (let [today (new Date)
        formatter (SimpleDateFormat. "dd-MM-yyyy")]
    (println "Ngày hôm nay là: " (.format formatter today))))

(formatted-current-date)
;; Đầu ra: Ngày hôm nay là:  05-04-2023
```

## Sâu hơn nữa

Clojure, một phương ngôn hiện đại của Lisp, cung cấp khả năng tương thích với Java, vì vậy chúng ta thường sử dụng API Ngày và Giờ phong phú của Java. Trong lịch sử, ngày được xử lý khá khác biệt – nghĩ về cơ cấu và đồng hồ mặt trời – nhưng trong lập trình, chúng ta đã có `Date` và `Calendar` của Java ngay từ JDK 1.0. Nay, chúng ta còn có `java.time` từ Java 8 cho một cách tiếp cận toàn diện và thống nhất hơn đối với dữ liệu thời gian.

Mặc dù `java.util.Date` phục vụ tốt cho các nhu cầu cơ bản, nó có những điểm lạ, như là có thể thay đổi sau khi tạo – ví dụ, với `setTime`. `java.time` là bất biến và linh hoạt hơn, nhưng cho những nhiệm vụ đơn giản như lấy ngày hiện tại, `Date` vẫn làm tốt công việc.

Các lựa chọn khác trong Clojure bao gồm các thư viện như clj-time, bao gồm Joda Time (tiền thân của `java.time`), và tick, một thư viện clojure hiện đại dành cho việc xử lý thời gian. Mỗi thứ có ưu và nhược điểm tùy thuộc vào phạm vi và độ phức tạp của nhu cầu xử lý thời gian của bạn.

Về mặt triển khai, việc lấy ngày-giờ hiện tại là một việc đơn giản trong Clojure do có gốc Java của nó. Thường thì đó là một dòng lệnh, mặc dù định dạng ngày đòi hỏi thêm một vài bước nữa và hiểu biết về các mẫu và chuẩn định dạng ngày của Java.

## Xem Thêm

Dưới đây là một số góc nhỏ thú vị trên web dành cho những người du hành thời gian Clojure tò mò:

- Tài liệu Clojure: https://clojuredocs.org/
- API Ngày/Giờ của Java 8: https://docs.oracle.com/javase/tutorial/datetime/
- Kho GitHub clj-time: https://github.com/clj-time/clj-time
- Kho GitHub tick: https://github.com/juxt/tick
