---
title:                "So sánh hai ngày"
date:                  2024-01-28T21:56:40.956853-07:00
model:                 gpt-4-0125-preview
simple_title:         "So sánh hai ngày"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/go/comparing-two-dates.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Điều gì & Tại sao?
So sánh hai ngày tức là kiểm tra chúng có mối quan hệ như thế nào: một ngày có trước, sau, hay trùng với ngày kia không? Lập trình viên thực hiện việc này để xử lý hạn chót, lên lịch sự kiện, hoặc theo dõi thời gian.

## Cách thực hiện:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	// Định nghĩa hai ngày
	date1 := time.Date(2023, time.April, 1, 0, 0, 0, 0, time.UTC)
	date2 := time.Now()

	// So sánh ngày
	if date1.Before(date2) {
		fmt.Println("Date1 trước Date2")
	} else if date1.After(date2) {
		fmt.Println("Date1 sau Date2")
	} else {
		fmt.Println("Date1 trùng với Date2")
	}
	
	// Lấy thời gian giữa hai ngày
	duration := date2.Sub(date1)
	fmt.Printf("Thời gian giữa hai ngày: %v\n", duration)
}
```

Kết quả mẫu khi chạy vào ngày 2 tháng 4 năm 2023:

```
Date1 trước Date2
Thời gian giữa hai ngày: 24h0m0s
```

## Khám phá sâu hơn
Ngày xửa ngày xưa, việc so sánh ngày trong lập trình là một cơn đau đầu — nghĩ về các phép tính rắc rối và việc sửa lỗi liên tục. Go làm cho nó đơn giản hơn với gói `time`. Các phương thức `Before()`, `After()`, và `Equal()` cho phép so sánh dễ dàng các đối tượng `Time`.

Bạn có các lựa chọn khác. Bạn có thể so sánh thủ công năm, tháng và ngày, nhưng đó là nhiều mã cho cùng một kết quả. Hoặc bạn có thể sử dụng thư viện bên thứ ba, mặc dù thư viện chuẩn của Go thường đủ dùng.

Về mặt kỹ thuật, `Sub()` cung cấp một loại `Duration` mà bạn có thể chuyển đổi thành giây, phút, giờ, hoặc thậm chí nanogiây. Nhớ rằng, múi giờ có thể khiến bạn lúng túng; luôn xem xét chúng khi so sánh ngày.

## Xem thêm

- Tài liệu gói thời gian của Go: [pkg.go.dev/time](https://pkg.go.dev/time)
- Go by Example - Định dạng và phân tích thời gian: [gobyexample.com/time-formatting-parsing](https://gobyexample.com/time-formatting-parsing)
