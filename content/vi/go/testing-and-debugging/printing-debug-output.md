---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:05:54.824549-07:00
description: "Trong l\u1EADp tr\xECnh m\xE1y t\xEDnh, \"In th\xF4ng \u0111i\u1EC7\
  p g\u1EE1 l\u1ED7i\" bao g\u1ED3m vi\u1EC7c s\u1EA3n xu\u1EA5t c\xE1c th\xF4ng \u0111\
  i\u1EC7p th\xF4ng tin chi ti\u1EBFt gi\xFAp c\xE1c nh\xE0 ph\xE1t tri\u1EC3n hi\u1EC3\
  u \u0111\u01B0\u1EE3c lu\u1ED3ng th\u1EF1c hi\u1EC7n\u2026"
lastmod: '2024-03-11T00:14:09.209454-06:00'
model: gpt-4-0125-preview
summary: "Trong l\u1EADp tr\xECnh m\xE1y t\xEDnh, \"In th\xF4ng \u0111i\u1EC7p g\u1EE1\
  \ l\u1ED7i\" bao g\u1ED3m vi\u1EC7c s\u1EA3n xu\u1EA5t c\xE1c th\xF4ng \u0111i\u1EC7\
  p th\xF4ng tin chi ti\u1EBFt gi\xFAp c\xE1c nh\xE0 ph\xE1t tri\u1EC3n hi\u1EC3u\
  \ \u0111\u01B0\u1EE3c lu\u1ED3ng th\u1EF1c hi\u1EC7n\u2026"
title: "In \u0111\u1EA7u ra debug"
---

{{< edit_this_page >}}

## Gì & Tại Sao?

Trong lập trình máy tính, "In thông điệp gỡ lỗi" bao gồm việc sản xuất các thông điệp thông tin chi tiết giúp các nhà phát triển hiểu được luồng thực hiện của chương trình hoặc xác định các vấn đề. Lập trình viên làm điều này để chẩn đoán và giải quyết các vấn đề một cách hiệu quả hơn, làm cho nó trở thành một kỹ năng thiết yếu trong bộ công cụ lập trình, bao gồm cả Go.

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
