---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:05:54.824549-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Trong Go, b\u1EA1n c\xF3 th\u1EC3 s\u1EED\
  \ d\u1EE5ng g\xF3i `fmt` chu\u1EA9n \u0111\u1EC3 in th\xF4ng \u0111i\u1EC7p g\u1EE1\
  \ l\u1ED7i ra b\u1EA3ng \u0111i\u1EC1u khi\u1EC3n. G\xF3i `fmt` cung c\u1EA5p m\u1ED9\
  t lo\u1EA1t c\xE1c h\xE0m, nh\u01B0\u2026"
lastmod: '2024-03-13T22:44:35.985665-06:00'
model: gpt-4-0125-preview
summary: "Trong Go, b\u1EA1n c\xF3 th\u1EC3 s\u1EED d\u1EE5ng g\xF3i `fmt` chu\u1EA9\
  n \u0111\u1EC3 in th\xF4ng \u0111i\u1EC7p g\u1EE1 l\u1ED7i ra b\u1EA3ng \u0111i\u1EC1\
  u khi\u1EC3n."
title: "In \u0111\u1EA7u ra debug"
weight: 33
---

## Cách thực hiện:
Trong Go, bạn có thể sử dụng gói `fmt` chuẩn để in thông điệp gỡ lỗi ra bảng điều khiển. Gói `fmt` cung cấp một loạt các hàm, như `Println`, `Printf`, và `Print`, phục vụ cho các nhu cầu định dạng khác nhau.

```go
package main

import (
	"fmt"
)

func main() {
	// Thông điệp đơn giản
	fmt.Println("Gỡ lỗi: Đang vào hàm main")

	var name = "Gopher"
	// Thông điệp có định dạng
	fmt.Printf("Xin chào, %s! Đây là một thông điệp gỡ lỗi.\n", name)

	// Sử dụng fmt.Print
	debugMsg := "Đây là một thông điệp gỡ lỗi khác."
	fmt.Print("Gỡ lỗi: ", debugMsg, "\n")
}
```

Kết quả mẫu:
```
Gỡ lỗi: Đang vào hàm main
Xin chào, Gopher! Đây là một thông điệp gỡ lỗi.
Gỡ lỗi: Đây là một thông điệp gỡ lỗi khác.
```

Đối với việc gỡ lỗi phức tạp hơn, gói `log` của Go có thể được sử dụng để bao gồm dấu thời gian và xuất ra các đích đến khác nhau, không chỉ là bảng điều khiển.

```go
package main

import (
	"log"
	"os"
)

func main() {
	// Tạo một tệp nhật ký
	file, err := os.OpenFile("debug.log", os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0666)
	if err != nil {
		log.Fatal("Lỗi khi tạo tệp nhật ký:", err)
	}
	defer file.Close()

	// Đặt đầu ra của nhật ký xuất ra tệp
	log.SetOutput(file)

	log.Println("Đây là một thông điệp gỡ lỗi với dấu thời gian.")
}
```

Thông điệp trong `debug.log` sẽ trông như thế này:
```
2023/04/01 15:00:00 Đây là một thông điệp gỡ lỗi với dấu thời gian.
```

## Sâu hơn
Việc in thông điệp gỡ lỗi đã là một phương thức lâu đời trong lập trình máy tính, với cách thực hiện thay đổi qua các ngôn ngữ khác nhau. Trong Go, các gói 'fmt' và 'log' của thư viện chuẩn cung cấp các lựa chọn đơn giản và đa dụng. Mặc dù gói 'fmt' đủ cho nhu cầu gỡ lỗi cơ bản, gói 'log' cung cấp chức năng nâng cao hơn như cấp độ nhật ký và đầu ra cấu hình linh hoạt.

Hơn nữa, khi ứng dụng trở nên phức tạp hơn, các framework nhật ký như 'zap' và 'logrus' có thể cung cấp các tính năng nâng cao hơn như nhật ký có cấu trúc và hiệu năng tốt hơn. Các gói của bên thứ ba này cho phép các nhà phát triển tùy chỉnh chiến lược nhật ký của họ theo nhu cầu cụ thể.

Tuy nhiên, việc tìm điểm cân bằng trong việc nhật ký là cần thiết. Thông điệp gỡ lỗi quá mức có thể làm lộn xộn nhật ký và làm cho việc tìm kiếm thông tin hữu ích trở nên khó khăn hơn. Các nhà phát triển nên xem xét sử dụng các cấp độ nhật ký khác nhau (ví dụ, debug, info, warn, error) để phân loại tầm quan trọng của các thông điệp, giúp nhật ký dễ dàng điều hướng và có ý nghĩa hơn.
