---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:58:10.050878-07:00
description: "Vi\u1EC7c l\u1EA5y ng\xE0y hi\u1EC7n t\u1EA1i trong Go l\xE0 m\u1ED9\
  t nhi\u1EC7m v\u1EE5 c\u01A1 b\u1EA3n d\xE0nh cho l\u1EADp tr\xECnh vi\xEAn, t\u01B0\
  \u01A1ng t\u1EF1 nh\u01B0 \"Hello, World!\" v\u1EC1 m\u1EE9c \u0111\u1ED9 ph\u1ED5\
  \ bi\u1EBFn. N\xF3 c\u1EA7n thi\u1EBFt cho c\xE1c\u2026"
lastmod: '2024-03-13T22:44:35.996560-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c l\u1EA5y ng\xE0y hi\u1EC7n t\u1EA1i trong Go l\xE0 m\u1ED9t nhi\u1EC7\
  m v\u1EE5 c\u01A1 b\u1EA3n d\xE0nh cho l\u1EADp tr\xECnh vi\xEAn, t\u01B0\u01A1\
  ng t\u1EF1 nh\u01B0 \"Hello, World!\" v\u1EC1 m\u1EE9c \u0111\u1ED9 ph\u1ED5 bi\u1EBF\
  n. N\xF3 c\u1EA7n thi\u1EBFt cho c\xE1c\u2026"
title: "L\u1EA5y ng\xE0y hi\u1EC7n t\u1EA1i"
weight: 29
---

## Cái gì và Tại sao?

Việc lấy ngày hiện tại trong Go là một nhiệm vụ cơ bản dành cho lập trình viên, tương tự như "Hello, World!" về mức độ phổ biến. Nó cần thiết cho các nhiệm vụ từ việc ghi nhật ký và đánh dấu thời gian cho các sự kiện, đến việc tính toán thời lượng và lập lịch cho các sự kiện tương lai.

## Làm thế nào:

Trong Go, gói `time` là cánh cửa của bạn để làm việc với ngày và giờ. Hàm `time.Now()` cung cấp cho bạn ngày và giờ hiện tại, trong khi các hàm và phương thức khác cho phép bạn định dạng hoặc thao tác với dữ liệu này. Dưới đây là cách lấy ngày hiện tại và các biểu diễn khác nhau của nó:

```go
package main

import (
	"fmt"
	"time"
)

func main() {
	currentTime := time.Now() // Lấy ngày và giờ hiện tại
	fmt.Println("Thời gian hiện tại:", currentTime)

	// Để lấy ngày theo định dạng YYYY-MM-DD
	fmt.Println("Ngày hiện tại:", currentTime.Format("2006-01-02"))

	// Để lấy các thành phần riêng lẻ của ngày
	nam, thang, ngay := currentTime.Date()
	fmt.Printf("Năm: %d, Tháng: %s, Ngày: %d\n", nam, thang, ngay)

	// Để lấy ngày trong tuần
	fmt.Println("Ngày trong tuần:", currentTime.Weekday())
}
```

Kết quả mẫu có thể trông như thế này:

```
Thời gian hiện tại: 2023-04-18 15:04:05.123456 +0000 UTC
Ngày hiện tại: 2023-04-18
Năm: 2023, Tháng: April, Ngày: 18
Ngày trong tuần: Tuesday
```

Chú ý cách `Format` sử dụng một ngày cụ thể (2006-01-02) làm chuỗi định dạng. Đây là ngày tham chiếu được Go chọn, phục vụ như một mẫu gợi nhớ cho việc định dạng ngày tháng.

## Sâu hơn

Quyết định sử dụng gói `time` để thao tác ngày và giờ trong Go phản ánh sự cam kết của ngôn ngữ này đối với các thư viện chuẩn mạnh mẽ và trực quan. Không giống như một số ngôn ngữ có thể có nhiều thư viện cạnh tranh hoặc phương pháp luận cho việc thao tác ngày tháng, Go ưu tiên có một tiêu chuẩn đơn lẻ, được tài liệu hóa tốt.

Sự lựa chọn kỳ lạ của ngày tham chiếu (`Mon Jan 2 15:04:05 MST 2006`) trong định dạng thời gian của Go, mặc dù ban đầu có thể gây nhầm lẫn, thực sự là một động thái tuyệt vời về tính hữu dụng. Nó cho phép lập trình viên biểu diễn các định dạng ngày và giờ thông qua cách tiếp cận dựa trên ví dụ, thay vì phải nhớ các ký tự hoặc biểu tượng mà các ngôn ngữ khác có thể sử dụng.

Tuy nhiên, mặc dù gói `time` cung cấp đầy đủ chức năng cho hầu hết nhu cầu, việc xử lý múi giờ và các thay đổi Giờ tiết kiệm ánh sáng ban ngày (DST) đôi khi có thể khiến các lập trình viên mới bước vào Go gặp khó khăn. Hiểu được cách Go xử lý thời gian cụ thể theo địa điểm là cực kỳ quan trọng để tránh các lỗi phổ biến trong việc thao tác thời gian.

Đối với nhu cầu lập lịch hoặc thao tác thời gian phức tạp hơn, các thư viện bên thứ ba như `github.com/robfig/cron` cho Go có thể cung cấp chức năng chuyên biệt hơn so với gói `time` chuẩn. Tuy nhiên, đối với hầu hết các ứng dụng yêu cầu lấy và xử lý ngày và giờ hiện tại, gói `time` cung cấp một điểm xuất phát vững chắc và đúng đắn trong Go.
