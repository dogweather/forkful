---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:57:17.307244-07:00
description: "Vi\u1EC7c t\xECm chi\u1EC1u d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i trong\
  \ Go l\xE0 vi\u1EC7c x\xE1c \u0111\u1ECBnh s\u1ED1 l\u01B0\u1EE3ng k\xFD t\u1EF1\
  \ m\xE0 n\xF3 ch\u1EE9a. C\xE1c l\u1EADp tr\xECnh vi\xEAn th\u01B0\u1EDDng xuy\xEA\
  n th\u1EF1c hi\u1EC7n thao t\xE1c n\xE0y \u0111\u1EC3 c\xF3 th\u1EC3 x\u1EED\u2026"
lastmod: '2024-03-11T00:14:09.192406-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c t\xECm chi\u1EC1u d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i trong Go\
  \ l\xE0 vi\u1EC7c x\xE1c \u0111\u1ECBnh s\u1ED1 l\u01B0\u1EE3ng k\xFD t\u1EF1 m\xE0\
  \ n\xF3 ch\u1EE9a. C\xE1c l\u1EADp tr\xECnh vi\xEAn th\u01B0\u1EDDng xuy\xEAn th\u1EF1\
  c hi\u1EC7n thao t\xE1c n\xE0y \u0111\u1EC3 c\xF3 th\u1EC3 x\u1EED\u2026"
title: "T\xECm ki\u1EBFm \u0111\u1ED9 d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i"
---

{{< edit_this_page >}}

## Gì và Tại sao?
Việc tìm chiều dài của một chuỗi trong Go là việc xác định số lượng ký tự mà nó chứa. Các lập trình viên thường xuyên thực hiện thao tác này để có thể xử lý chuỗi một cách hiệu quả, cho dù đó là để kiểm tra tính hợp lệ, trích xuất chuỗi con, hay chỉ đơn giản là để áp đặt các ràng buộc trong đầu vào của người dùng.

## Cách thức:
Trong Go, chuỗi được xem như các chuỗi byte không thể thay đổi. Bạn có thể tìm chiều dài của một chuỗi bằng cách sử dụng hàm được tích hợp sẵn `len()` mà trả về số lượng byte, không nhất thiết là số lượng ký tự. Dưới đây là cách sử dụng nó:

```go
package main

import (
	"fmt"
	"unicode/utf8"
)

func main() {
	// Sử dụng len() để tìm chiều dài byte
	str := "Hello, 世界"
	byteLength := len(str)
	fmt.Println("Chiều Dài Byte:", byteLength) // Kết quả: Chiều Dài Byte: 13

	// Để lấy chính xác số lượng ký tự hoặc runes trong một chuỗi
	runeLength := utf8.RuneCountInString(str)
	fmt.Println("Chiều Dài Rune:", runeLength) // Kết quả: Chiều Dài Rune: 9
}
```
Phương pháp đầu tiên sử dụng `len()` có thể sẽ không luôn cho kết quả mong đợi vì nó đếm byte. Đối với các chuỗi chứa ký tự không phải ASCII (như "世界"), `RuneCountInString` từ gói `unicode/utf8` nên được sử dụng thay thế để đếm chính xác điểm mã Unicode.

## Sâu hơn
Trước Go 1, không có sự phân biệt rõ ràng trong việc xử lý chuỗi như là chuỗi byte so với chuỗi ký tự. Sau Go 1, việc áp dụng UTF-8 làm chuẩn mã hóa cho chuỗi yêu cầu phải có cách tiếp cận rõ ràng hơn. Hàm `len()` hoạt động hoàn hảo cho các chuỗi ASCII, nơi mà các ký tự được biểu diễn trong một byte duy nhất. Tuy nhiên, khi các ứng dụng Go trở nên toàn cầu hơn và nhu cầu hỗ trợ đa dạng ngôn ngữ và bộ ký tự tăng lên, cách tiếp cận đơn giản của `len()` đã hiển thị những hạn chế.

Việc giới thiệu và sử dụng `utf8.RuneCountInString()` giải quyết các hạn chế này bằng cách cung cấp một cách để đếm ký tự Unicode thực sự (rune trong thuật ngữ của Go). Phương pháp này đảm bảo rằng việc tính toán chiều dài không phụ thuộc vào đặc điểm mã hóa cụ thể của UTF-8, nơi mà ký tự có thể kéo dài qua nhiều byte.

Một cách tiếp cận khác cho việc duyệt và xử lý chuỗi, phù hợp hơn với tinh thần đồng thời và hiệu quả của Go, có thể liên quan đến việc xử lý chuỗi như là các lát cắt của rune. Tuy nhiên, phương pháp này yêu cầu một bước chuyển đổi và không ngay lập tức giải quyết tất cả các phức tạp của Unicode (ví dụ, các ký tự kết hợp).

Tóm lại, trong khi `len()` phù hợp cho chiều dài byte và hiệu quả cho văn bản ASCII, `utf8.RuneCountInString()` là lựa chọn đáng tin cậy hơn cho một ứng dụng tương thích toàn cầu. Dẫu vậy, các nhà phát triển được khuyến khích hiểu các sự đánh đổi về hiệu suất và việc sử dụng bộ nhớ mà những lựa chọn này đòi hỏi.
