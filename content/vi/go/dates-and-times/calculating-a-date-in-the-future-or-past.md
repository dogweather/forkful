---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:53:13.367205-07:00
description: "Vi\u1EC7c t\xEDnh to\xE1n m\u1ED9t ng\xE0y trong t\u01B0\u01A1ng lai\
  \ ho\u1EB7c qu\xE1 kh\u1EE9 trong Go li\xEAn quan \u0111\u1EBFn vi\u1EC7c thao t\xE1\
  c c\xE1c gi\xE1 tr\u1ECB ng\xE0y v\xE0 gi\u1EDD \u0111\u1EC3 x\xE1c \u0111\u1ECB\
  nh m\u1ED9t \u0111i\u1EC3m c\u1EE5 th\u1EC3 so v\u1EDBi m\u1ED9t\u2026"
lastmod: '2024-03-13T22:44:36.000677-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c t\xEDnh to\xE1n m\u1ED9t ng\xE0y trong t\u01B0\u01A1ng lai ho\u1EB7\
  c qu\xE1 kh\u1EE9 trong Go li\xEAn quan \u0111\u1EBFn vi\u1EC7c thao t\xE1c c\xE1\
  c gi\xE1 tr\u1ECB ng\xE0y v\xE0 gi\u1EDD \u0111\u1EC3 x\xE1c \u0111\u1ECBnh m\u1ED9\
  t \u0111i\u1EC3m c\u1EE5 th\u1EC3 so v\u1EDBi m\u1ED9t\u2026"
title: "T\xEDnh to\xE1n ng\xE0y trong t\u01B0\u01A1ng lai ho\u1EB7c qu\xE1 kh\u1EE9"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Việc tính toán một ngày trong tương lai hoặc quá khứ trong Go liên quan đến việc thao tác các giá trị ngày và giờ để xác định một điểm cụ thể so với một ngày đã cho. Các lập trình viên thường thực hiện nhiệm vụ này cho các ứng dụng cần lịch trình, hạn chót, nhắc nhở, hoặc bất kỳ chức năng nào mà tiến triển hoặc lùi lại thời gian là thiết yếu.

## Cách thực hiện:

Go cung cấp gói `time` để xử lý các hoạt động ngày và giờ, đưa ra các cơ chế đơn giản để thêm hoặc bớt thời gian. Dưới đây là cách tận dụng gói `time` để tính toán các ngày trong tương lai hoặc quá khứ:

```go
package main

import (
	"fmt"
	"time"
)

func main() {
	// Ngày và giờ hiện tại
	now := time.Now()
	fmt.Println("Ngày và giờ hiện tại: ", now)

	// Tính toán một ngày 10 ngày trong tương lai
	futureDate := now.AddDate(0, 0, 10)
	fmt.Println("Ngày 10 ngày trong tương lai: ", futureDate)
	
	// Tính toán một ngày 30 ngày trong quá khứ
	pastDate := now.AddDate(0, 0, -30)
	fmt.Println("Ngày 30 ngày trong quá khứ: ", pastDate)
	
	// Thêm 5 giờ và 30 phút vào ngày và giờ hiện tại
	futureTime := now.Add(5*time.Hour + 30*time.Minute)
	fmt.Println("Thời gian tương lai (sau 5 giờ và 30 phút): ", futureTime)
}
```

Kết quả mẫu:
```
Ngày và giờ hiện tại:  2023-04-01 15:04:05.123456789 +0000 UTC
Ngày 10 ngày trong tương lai:  2023-04-11 15:04:05.123456789 +0000 UTC
Ngày 30 ngày trong quá khứ:  2023-03-02 15:04:05.123456789 +0000 UTC
Thời gian tương lai (sau 5 giờ và 30 phút):  2023-04-01 20:34:05.123456789 +0000 UTC
```
Lưu ý cách sử dụng phương thức `AddDate` để thao tác ngày theo năm, tháng và ngày, trong khi phương thức `Add` được sử dụng cho các khoảng thời gian chính xác hơn như giờ, phút và giây.

## Sâu hơn nữa

Gói `time` của ngôn ngữ lập trình Go hỗ trợ thao tác thời gian với độ an toàn kiểu mạnh và cú pháp rõ ràng, những đặc trưng mà Go được đánh giá cao. Việc triển khai này dựa vào các chức năng thao tác thời gian được cung cấp bởi hệ điều hành cơ bản, đảm bảo hiệu quả và chính xác. Lịch sử, việc xử lý ngày và giờ trong lập trình đã gặp nhiều phức tạp do sự khác biệt trong múi giờ, năm nhuận, và thay đổi giờ tiết kiệm ánh sáng. Gói `time` của Go tóm gọn nhiều phức tạp này, cung cấp cho các nhà phát triển một bộ công cụ mạnh mẽ để thao tác thời gian.

Mặc dù gói `time` nguyên bản của Go bao phủ một phạm vi rộng lớn các nhu cầu thao tác thời gian, nhưng các thư viện thay thế như `github.com/jinzhu/now` cung cấp thêm tiện ích và chức năng cho các trường hợp sử dụng cụ thể hơn. Những thư viện thay thế này có thể đặc biệt hữu ích cho nhu cầu thao tác ngày và giờ phức tạp hơn không được hỗ trợ trực tiếp bởi gói `time` nguyên bản.

Tuy nhiên, đối với hầu hết các ứng dụng, khả năng thao tác thời gian tích hợp của Go cung cấp một nền tảng vững chắc. Chúng cân bằng hiệu suất với sự dễ sử dụng, đảm bảo rằng các nhà phát triển có thể xử lý hầu hết các nhiệm vụ liên quan đến thời gian một cách hiệu quả mà không cần tới các gói bên thứ ba.
