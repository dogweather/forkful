---
title:                "Ghi log"
date:                  2024-01-28T22:03:43.230633-07:00
model:                 gpt-4-0125-preview
simple_title:         "Ghi log"
programming_language: "Go"
category:             "Go"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/go/logging.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Điều Gì & Tại Sao?
Việc ghi nhật ký liên quan đến việc giữ lại bản ghi của các sự kiện, trạng thái và luồng dữ liệu trong một ứng dụng. Lập trình viên thực hiện điều này để chẩn đoán lỗi, giám sát hiệu suất và theo dõi sức khỏe hoạt động của ứng dụng—cơ bản làm cho nó tương đương với hộp đen trong máy bay.

## Làm Thế Nào:
Trong Go, việc ghi nhật ký có thể được xử lý theo nhiều cách, từ gói `log` của thư viện chuẩn đến các thư viện bên thứ ba như `logrus` và `zap`. Dưới đây là một ví dụ đơn giản sử dụng gói `log` có sẵn:

```Go
package main

import (
	"log"
	"os"
)

func main() {
	// Tạo một tệp nhật ký
	logFile, err := os.OpenFile("app.log", os.O_CREATE|os.O_APPEND|os.O_WRONLY, 0666)
	if err != nil {
		log.Fatal(err)
	}
	defer logFile.Close()

	// Đặt đầu ra nhật ký vào tệp
	log.SetOutput(logFile)

	// Ghi một số sự kiện
	log.Println("Khởi động ứng dụng...")
	// ... logic ứng dụng ở đây ...
	log.Println("Ứng dụng kết thúc thành công.")
}
```

Nếu bạn chạy mã này, bạn sẽ không thấy bất kỳ đầu ra nào tới terminal bởi vì tất cả đều được ghi vào `app.log`. Dưới đây là cái nhìn qua những gì bạn sẽ tìm thấy bên trong tệp nhật ký đó:

```
2023/01/02 15:04:05 Khởi động ứng dụng...
2023/01/02 15:05:01 Ứng dụng kết thúc thành công.
```

## Sâu Hơn
Việc ghi nhật ký trong lập trình có từ những máy tính đầu tiên, nơi mà các kỹ sư thực sự tìm thấy bọ (bướm đêm, cụ thể hơn) bị nghiền nát trong phần cứng, và họ đã ghi chúng lại! Tiến xa đến ngày nay, và việc ghi nhật ký đã trở thành một cách tinh vi để hiểu những gì đang xảy ra bên trong các hệ thống phức tạp.

Trong khi gói `log` trong Go khá đơn giản, nó có thể đủ cho các ứng dụng cơ bản. Tuy nhiên, trong bối cảnh của các hệ thống phân tán hiện đại, hoặc khi bạn cần kiểm soát tinh tế hơn đối với đầu ra nhật ký của mình (như các mức độ nghiêm trọng khác nhau), bạn có thể muốn khám phá các giải pháp mạnh mẽ hơn.

Các thư viện ghi nhật ký bên thứ ba như `logrus` và `zap` cung cấp ghi nhật ký có cấu trúc, có nghĩa là bạn có thể ghi các kiểu dữ liệu phức tạp như JSON, làm cho việc giải thích nhật ký dễ dàng hơn, đặc biệt khi kết hợp với các hệ thống quản lý nhật ký như ELK Stack hoặc Splunk.

Khi cân nhắc việc thực hiện chiến lược ghi nhật ký, cũng rất cần thiết để suy nghĩ về hậu quả về hiệu suất. Các thư viện ghi nhật ký có hiệu suất cao được tối ưu hóa để giảm ảnh hưởng đến throughput và độ trễ của ứng dụng. Ví dụ, `zap` tự hào về thiết kế nhanh chóng, ít cấp phát, có thể rất quan trọng cho các hệ thống thời gian thực.

Ngoài các thư viện khác nhau, các định dạng và tiêu chuẩn ghi nhật ký cũng đáng được chú ý. Các định dạng ghi nhật ký có cấu trúc như JSON có thể cực kỳ mạnh mẽ khi được sử dụng kết hợp với các hệ thống xử lý nhật ký. Ngược lại, nhật ký văn bản thuần túy dễ đọc cho con người nhưng khó phân tích một cách tự động hơn.

## Tham Khảo Thêm
Để hiểu sâu hơn về khả năng ghi nhật ký của Go, những tài nguyên sau có thể hữu ích:

- Blog Go về ghi nhật ký: https://blog.golang.org/logging
- `logrus`, một trình ghi nhật ký có cấu trúc cho Go: https://github.com/sirupsen/logrus
- `zap`, một trình ghi nhật ký cấp độ, có cấu trúc, nhanh chóng: https://github.com/uber-go/zap
- ELK Stack (Elasticsearch, Logstash, Kibana) cho phân tích nhật ký: https://www.elastic.co/what-is/elk-stack
- So sánh các thư viện ghi nhật ký của Go: https://www.loggly.com/blog/benchmarking-5-popular-golang-logging-libraries/
