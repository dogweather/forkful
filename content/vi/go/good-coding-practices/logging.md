---
title:                "Ghi nhật ký"
aliases:
- /vi/go/logging/
date:                  2024-02-03T17:59:33.176511-07:00
model:                 gpt-4-0125-preview
simple_title:         "Ghi nhật ký"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/go/logging.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?

Trong phát triển phần mềm, việc ghi nhật ký (logging) là quy trình ghi lại thông tin về quá trình thực thi của chương trình, được thiết kế để theo dõi hành vi và chẩn đoán các vấn đề. Các lập trình viên thực hiện việc ghi nhật ký để giám sát hiệu suất phần mềm, gỡ lỗi, và đảm bảo an ninh hệ thống cũng như tuân thủ, làm cho nó trở thành công cụ không thể thiếu cho bảo trì và phân tích ứng dụng.

## Làm thế nào:

Trong Go, việc ghi nhật ký có thể được thực hiện sử dụng gói thư viện chuẩn `log`. Gói này cung cấp khả năng ghi nhật ký đơn giản, chẳng hạn như ghi vào đầu ra chuẩn hay vào các tệp. Hãy bắt đầu với một ví dụ cơ bản về việc ghi nhật ký vào đầu ra chuẩn:

```go
package main

import (
	"log"
)

func main() {
	log.Println("Đây là một mục nhật ký cơ bản.")
}
```

Đầu ra:
```
2009/11/10 23:00:00 Đây là một mục nhật ký cơ bản.
```

Dấu thời gian ở đầu mục nhật ký được tự động thêm vào bởi gói `log`. Tiếp theo, hãy tìm hiểu cách ghi nhật ký vào tệp thay vì đầu ra chuẩn:

```go
package main

import (
	"log"
	"os"
)

func main() {
	file, err := os.OpenFile("app.log", os.O_CREATE|os.O_APPEND|os.O_WRONLY, 0666)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	log.SetOutput(file)
	log.Println("Mục nhật ký này đi vào một tệp.")
}
```

Bây giờ, hãy thực hiện một trường hợp sử dụng nâng cao hơn: tùy chỉnh định dạng nhật ký. Go cho phép bạn tạo một bộ ghi nhật ký tùy chỉnh với `log.New()`:

```go
package main

import (
	"log"
	"os"
)

func main() {
	logger := log.New(os.Stdout, "NHẬT KÝ TÙY CHỈNH: ", log.Ldate|log.Ltime|log.Lshortfile)
	logger.Println("Đây là một thông điệp nhật ký tùy chỉnh.")
}
```

Đầu ra:
```
NHẬT KÝ TÙY CHỈNH: 2009/11/10 23:00:00 main.go:11: Đây là một thông điệp nhật ký tùy chỉnh.
```

Ví dụ này thêm tiền tố "NHẬT KÝ TÙY CHỈNH: " vào mỗi thông điệp nhật ký và bao gồm ngày, giờ và vị trí tệp nguồn.

## Đi sâu hơn

Gói `log` của thư viện chuẩn Go là đơn giản và đủ cho nhiều ứng dụng, nhưng nó thiếu một số tính năng tiên tiến hơn được tìm thấy trong các thư viện ghi nhật ký của bên thứ ba, chẳng hạn như ghi nhật ký có cấu trúc, quay vòng nhật ký, và ghi nhật ký dựa trên cấp độ. Các gói như `zap` và `logrus` cung cấp những tính năng tiên tiến này và được cộng đồng Go đánh giá cao về hiệu suất và tính linh hoạt của chúng.

Ví dụ, ghi nhật ký có cấu trúc cho phép bạn ghi dữ liệu theo một định dạng có cấu trúc (như JSON), rất hữu ích cho các ứng dụng dựa trên đám mây hiện đại nơi các nhật ký có thể được phân tích bằng các công cụ hoặc dịch vụ khác nhau. `zap`, cụ thể là được biết đến với hiệu suất cao và gánh nặng phân bổ thấp, làm cho nó phù hợp cho các ứng dụng nơi tốc độ và hiệu quả là quan trọng.

Theo lịch sử, việc ghi nhật ký trong Go đã phát triển đáng kể kể từ khi ngôn ngữ này được ra đời. Các phiên bản đầu của Go cung cấp khả năng ghi nhật ký cơ bản mà chúng ta thấy trong gói `log`. Tuy nhiên, khi ngôn ngữ này tăng trưởng về phổ biến và độ phức tạp của các ứng dụng được viết bằng Go tăng lên, cộng đồng bắt đầu phát triển thêm các thư viện ghi nhật ký phức tạp hơn để đáp ứng nhu cầu của họ. Ngày nay, mặc dù gói `log` tiêu chuẩn vẫn là một lựa chọn khả thi cho các ứng dụng đơn giản, nhiều nhà phát triển chuyển sang những giải pháp bên thứ ba cho nhu cầu ghi nhật ký phức tạp hơn.
