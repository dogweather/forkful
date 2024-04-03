---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:12.210222-07:00
description: "Ghi ch\xE9p (Logging) qu\u1EA3 th\u1EF1c t\u01B0\u01A1ng \u0111\u01B0\
  \u01A1ng v\u1EDBi nh\u1EADt k\xFD t\xE0u th\u1EE7y; n\xF3 l\xE0 c\xE1ch \u0111\u1EC3\
  \ ghi l\u1EA1i c\xE1c s\u1EF1 ki\u1EC7n x\u1EA3y ra khi m\u1ED9t \u1EE9ng d\u1EE5\
  ng \u0111ang ch\u1EA1y. C\xE1c l\u1EADp tr\xECnh vi\xEAn l\xE0m\u2026"
lastmod: '2024-03-13T22:44:36.161728-06:00'
model: gpt-4-0125-preview
summary: "Ghi ch\xE9p (Logging) qu\u1EA3 th\u1EF1c t\u01B0\u01A1ng \u0111\u01B0\u01A1\
  ng v\u1EDBi nh\u1EADt k\xFD t\xE0u th\u1EE7y; n\xF3 l\xE0 c\xE1ch \u0111\u1EC3 ghi\
  \ l\u1EA1i c\xE1c s\u1EF1 ki\u1EC7n x\u1EA3y ra khi m\u1ED9t \u1EE9ng d\u1EE5ng\
  \ \u0111ang ch\u1EA1y."
title: Ghi log
weight: 17
---

## Làm thế nào:
Clojure dựa vào các tiện ích ghi chép của Java, nhưng bạn có thể vận dụng chúng theo cách Clojure tiếp cận hợp lý hơn. Hãy cùng xem bạn có thể sử dụng `clojure.tools.logging` như thế nào, nó cung cấp một trừu tượng đơn giản trên nhiều khung ghi chép khác nhau:

Đầu tiên, thêm một phụ thuộc cho `clojure.tools.logging` và một triển khai ghi chép như `log4j` trong `project.clj` của bạn:

```clojure
:dependencies [[org.clojure/clojure "1.10.3"]
               [org.clojure/tools.logging "1.1.0"]
               [log4j/log4j "1.2.17"]]
```

Bây giờ, hãy log một số thông điệp:

```clojure
(require '[clojure.tools.logging :as log])

(defn compute-answer-to-everything []
  (log/debug "Bắt đầu tính toán kỹ lưỡng...")
  (Thread/sleep 3000) ; Mô phỏng một quy trình tính toán lâu dài
  (log/info "Tính toán hoàn tất. Câu trả lời là 42.")
  42)

(compute-answer-to-everything)
```
Đầu ra sẽ không hiển thị thông điệp `DEBUG` mặc định, vì mức độ log thường được thiết lập là `INFO`:

```
INFO  [your-namespace] - Tính toán hoàn tất. Câu trả lời là 42.
```

Bạn có thể cấu hình mức độ log và các bộ thêm vào trong một tệp `log4j.properties` để nhận được đầu ra chi tiết hơn nếu cần.

## Sâu hơn
`clojure.tools.logging` của Clojure đã tồn tại một thời gian và đóng vai trò như một cầu nối giữa mã Clojure và thế giới ghi chép của Java. Trong lịch sử, Java đã trải qua nhiều lần lặp lại và thư viện ghi chép như API ghi chép tích hợp của Java, `log4j`, `slf4j`, và `logback`.

Trong Clojure, mặc dù bạn có thể sử dụng trực tiếp các khung ghi chép Java, `clojure.tools.logging` sẽ phát hiện và ủy thác đến bất kỳ khung ghi chép nào nó tìm thấy trên classpath của bạn, giúp bạn tránh được việc phụ thuộc chặt chẽ vào một triển khai cụ thể. Điều này có thể giúp mã Clojure của bạn có tính di động và mô-đun hơn.

Những phương án thay thế cho `clojure.tools.logging` trong hệ sinh thái Clojure bao gồm các thư viện như `timbre`, là một thư viện ghi chép Clojure thuần túy với các tính năng như xoay log, lọc và ghi chép không đồng bộ ngay lập tức.

Chi tiết thực hiện là quan trọng khi nói đến ghi chép trong một môi trường đa luồng như Clojure. Tại đây, tính bất biến và quản lý tác động phụ mang lại những lợi ích rõ ràng. Ghi chép, như một tác động phụ, nên được xử lý cẩn thận để tránh nghẽn cổ chai hiệu suất và đảm bảo an toàn luồng, điều mà hầu hết các khung ghi chép Java đã chăm sóc.

Cuối cùng, hãy xem xét ghi chép có cấu trúc, nơi các log được viết dưới dạng dữ liệu có cấu trúc (như JSON). Điều này có thể vô cùng hữu ích cho phân tích và xử lý sau này, đặc biệt khi đối phó với các hệ thống phân tán quy mô lớn.

## Xem Thêm
Nếu bạn muốn tìm hiểu thêm, hãy xem các nguồn tài nguyên sau:

- Tài liệu Clojure Tools Logging: https://github.com/clojure/tools.logging
- Timbre, một thư viện ghi chép Clojure: https://github.com/ptaoussanis/timbre
- Cấu hình Log4J trong Clojure: http://clojure-doc.org/articles/tutorials/logging_with_log4j.html
- Hướng dẫn Logback cho các cài đặt nâng cao: http://logback.qos.ch/manual/
- Hướng dẫn về ghi chép có cấu trúc trong Clojure: https://corfield.org/blog/2020/04/28/structured-logging/
