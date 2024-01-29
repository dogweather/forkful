---
title:                "Lấy ngày hiện tại"
date:                  2024-01-28T22:01:30.756540-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lấy ngày hiện tại"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/go/getting-the-current-date.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Lấy ngày hiện tại trong Go bao gồm việc lấy ngày của thời điểm hiện tại từ hệ thống. Các lập trình viên theo dõi thời gian để ghi dấu thời gian cho các sự kiện, lên lịch cho các nhiệm vụ, hoặc đơn giản là hiển thị ngày hiện tại cho người dùng.

## Làm cách nào:

Dưới đây là cách bạn lấy ngày hiện tại trong Go:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	currentTime := time.Now()
	fmt.Println("Ngày Hiện Tại là:", currentTime.Format("2006-01-02"))
}
```

Nếu bạn chạy đoạn mã này, bạn sẽ nhận được đầu ra tương tự như:
```
Ngày Hiện Tại là: 2023-04-05
```
Định dạng `"2006-01-02"` có thể trông lạ, nhưng trong Go, chuỗi cụ thể này là bảng mẫu tham chiếu cho việc định dạng ngày.

## Sâu hơn

Trước Go, mỗi ngôn ngữ lập trình đều có cách quản lý ngày và giờ độc đáo của riêng mình. Go đơn giản hóa điều này, phần lớn. Tại sao lại là số `2006-01-02 15:04:05`? Chúng là một kỷ niệm mnemonic—từ số 1 đến số 7 theo thứ tự tăng dần, ánh xạ tới `năm-tháng-ngày giờ:phút:giây`.

Có những phương án thay thế như sử dụng thời gian Unix (số giây kể từ ngày 1 tháng 1 năm 1970) nhưng nó ít dễ đọc cho con người hơn. Gói `time` trong Go cung cấp tiện ích cho cả hai định dạng và thêm vào đó, xem xét các múi giờ và tiết kiệm ánh sáng ban ngày. Bên dưới cùng, `time.Now()` giao tiếp với đồng hồ hệ thống của bạn để lấy ngày và giờ hiện tại.

## Xem Thêm

Để biết thêm chi tiết về gói `time` của Go, hãy kiểm tra tài liệu chính thức:
- Gói `time` của Go: https://pkg.go.dev/time

Để đi sâu vào việc định dạng và phân tích ngày trong Go:
- Go by Example - Định dạng / Phân tích Thời gian: https://gobyexample.com/time-formatting-parsing

Hiểu về múi giờ trong lập trình Go:
- Làm việc với Múi Giờ trong Go: https://www.alexedwards.net/blog/working-with-time-zones
