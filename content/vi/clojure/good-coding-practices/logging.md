---
title:                "Ghi log"
aliases: - /vi/clojure/logging.md
date:                  2024-01-28T22:03:12.210222-07:00
model:                 gpt-4-0125-preview
simple_title:         "Ghi log"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/clojure/logging.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Ghi chép (Logging) quả thực tương đương với nhật ký tàu thủy; nó là cách để ghi lại các sự kiện xảy ra khi một ứng dụng đang chạy. Các lập trình viên làm điều này để theo dõi các sự kiện này cho việc gỡ lỗi, kiểm toán, hoặc để hiểu rõ hơn về hành vi của hệ thống khi ở trong môi trường sản xuất.

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
