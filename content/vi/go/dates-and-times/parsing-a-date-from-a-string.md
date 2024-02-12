---
title:                "Phân tích ngày tháng từ một chuỗi"
aliases:
- /vi/go/parsing-a-date-from-a-string/
date:                  2024-02-03T18:05:34.870681-07:00
model:                 gpt-4-0125-preview
simple_title:         "Phân tích ngày tháng từ một chuỗi"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/go/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Phân tích cú pháp một ngày từ một chuỗi trong Go bao gồm việc chuyển đổi ngày được biểu diễn dưới dạng văn bản sang một định dạng có thể sử dụng hơn (ví dụ, `time.Time`). Các lập trình viên thực hiện công việc này để xử lý dữ liệu ngày và giờ một cách chính xác hơn trong các ứng dụng, đặc biệt khi phải đối mặt với đầu vào từ người dùng, API hoặc hệ thống lưu trữ nơi mà ngày thường được biểu diễn dưới dạng chuỗi.

## Làm thế nào:

Go cung cấp sự hỗ trợ mạnh mẽ để phân tích cú pháp ngày và giờ thông qua gói `time`. Chìa khóa là hiểu về định dạng ngày tham chiếu của Go: `Mon Jan 2 15:04:05 MST 2006`, mà bạn sử dụng để nói với Go cách diễn giải chuỗi đầu vào. Dưới đây là một ví dụ nhanh để bạn bắt đầu:

```go
package main

import (
	"fmt"
	"time"
)

func main() {
	// Ví dụ chuỗi ngày
	dateStr := "2023-04-12 14:45:00"
	
	// Xác định layout/định dạng của chuỗi ngày đầu vào
	// Layout này nói với Go rằng cần mong đợi một năm, tiếp theo là tháng, 
	// sau đó là ngày, giờ, phút và cuối cùng là giây
	layout := "2006-01-02 15:04:05"
	
	// Phân tích cú pháp chuỗi ngày theo layout
	parsedDate, err := time.Parse(layout, dateStr)
	if err != nil {
		fmt.Println("Lỗi khi phân tích cú pháp ngày:", err)
		return
	}
	
	// Xuất ngày đã phân tích
	fmt.Println("Ngày đã phân tích cú pháp:", parsedDate)
}
```

Khi bạn chạy đoạn mã này, bạn sẽ nhận được:

```
Ngày đã phân tích cú pháp: 2023-04-12 14:45:00 +0000 UTC
```

Chú ý cách chuỗi `layout` sử dụng các giá trị ngày tham chiếu để chỉ định định dạng của chuỗi đầu vào. Điều chỉnh `layout` để phù hợp với định dạng của các ngày đầu vào của bạn.

## Tìm hiểu sâu

Thiết kế của việc phân tích cú pháp ngày và giờ trong Go độc đáo, sử dụng một ngày tham chiếu cụ thể (`Mon Jan 2 15:04:05 MST 2006`). Phương pháp này, thay vì sử dụng các bộ chọn định dạng thông thường hơn (như `YYYY` cho năm), đã được chọn cho sự dễ đọc và dễ sử dụng, tận dụng một định dạng dựa trên ví dụ. 

Mặc dù ban đầu có vẻ không quen thuộc với các lập trình viên đã quen sử dụng ngôn ngữ khác, nhiều người tìm thấy nó trực quan hơn sau một thời gian ngắn điều chỉnh. Đối với các ứng dụng cần sự thao tác ngày phức tạp hơn hoặc định dạng không được hỗ trợ trực tiếp bởi gói `time` của Go, các thư viện bên thứ ba như `github.com/jinzhu/now` có thể cung cấp thêm chức năng. Tuy nhiên, đối với phần lớn các ứng dụng tiêu chuẩn, khả năng tích hợp sẵn của Go là mạnh mẽ, hiệu suất cao và đặc trưng, thể hiện triết lý của Go về sự đơn giản và rõ ràng.
