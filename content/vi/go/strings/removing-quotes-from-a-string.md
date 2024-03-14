---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:07:49.356145-07:00
description: "Vi\u1EC7c lo\u1EA1i b\u1ECF d\u1EA5u nh\xE1y kh\u1ECFi m\u1ED9t chu\u1ED7\
  i trong Go l\xE0 qu\xE1 tr\xECnh x\xF3a b\u1ECF c\xE1c d\u1EA5u nh\xE1y \u0111\u1EA7\
  u v\xE0 cu\u1ED1i (`\"` ho\u1EB7c `'`) kh\u1ECFi m\u1ED9t chu\u1ED7i cho tr\u01B0\
  \u1EDBc. C\xE1c l\u1EADp tr\xECnh vi\xEAn\u2026"
lastmod: '2024-03-13T22:44:35.964901-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c lo\u1EA1i b\u1ECF d\u1EA5u nh\xE1y kh\u1ECFi m\u1ED9t chu\u1ED7\
  i trong Go l\xE0 qu\xE1 tr\xECnh x\xF3a b\u1ECF c\xE1c d\u1EA5u nh\xE1y \u0111\u1EA7\
  u v\xE0 cu\u1ED1i (`\"` ho\u1EB7c `'`) kh\u1ECFi m\u1ED9t chu\u1ED7i cho tr\u01B0\
  \u1EDBc. C\xE1c l\u1EADp tr\xECnh vi\xEAn\u2026"
title: "Lo\u1EA1i b\u1ECF d\u1EA5u ngo\u1EB7c k\xE9p kh\u1ECFi chu\u1ED7i"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Việc loại bỏ dấu nháy khỏi một chuỗi trong Go là quá trình xóa bỏ các dấu nháy đầu và cuối (`"` hoặc `'`) khỏi một chuỗi cho trước. Các lập trình viên thường cần thực hiện nhiệm vụ này để làm sạch đầu vào của người dùng, phân tích dữ liệu văn bản một cách hiệu quả hơn, hoặc chuẩn bị chuỗi cho các bước xử lý tiếp theo mà yêu cầu nội dung không chứa dấu nháy.

## Làm thế nào:

Go cung cấp một số cách tiếp cận để loại bỏ dấu nháy khỏi chuỗi, nhưng một trong những phương pháp đơn giản nhất là sử dụng các hàm `Trim` và `TrimFunc` được cung cấp bởi gói `strings`. Dưới đây là cách làm:

```go
package main

import (
	"fmt"
	"strings"
	"unicode"
)

func main() {
	quotedString := `"Đây là một chuỗi 'được trích dẫn'"`

	// Sử dụng strings.Trim để loại bỏ các dấu nháy cụ thể
	unquoted := strings.Trim(quotedString, `"'`)
	fmt.Println("Sử dụng strings.Trim:", unquoted)

	// Phương pháp tự chọn sử dụng strings.TrimFunc để kiểm soát nhiều hơn
	unquotedFunc := strings.TrimFunc(quotedString, func(r rune) bool {
		return r == '"' || r == '\''
	})
	fmt.Println("Sử dụng strings.TrimFunc:", unquotedFunc)
}
```

Ví dụ này trình bày hai cách tiếp cận để loại bỏ cả dấu nháy kép (`"`) và dấu nháy đơn (`'`). Hàm `strings.Trim` là đơn giản hơn và làm việc tốt khi bạn biết chính xác những ký tự nào cần loại bỏ. Mặt khác, `strings.TrimFunc` cung cấp nhiều tính linh hoạt hơn, cho phép bạn chỉ định một hàm tự chọn để quyết định ký tự nào được loại bỏ. Đầu ra mẫu của đoạn mã trên là:

```
Sử dụng strings.Trim: Đây là một chuỗi 'được trích dẫn'
Sử dụng strings.TrimFunc: Đây là một chuỗi 'được trích dẫn'
```

Cả hai phương pháp đều loại bỏ các dấu nháy đầu và cuối khỏi chuỗi một cách hiệu quả.

## Sâu hơn

Các hàm `Trim` và `TrimFunc` từ gói `strings` là một phần của thư viện chuẩn rộng lớn của Go, được thiết kế để cung cấp khả năng xử lý chuỗi mạnh mẽ nhưng vẫn đơn giản mà không cần đến các gói bên thứ ba. Sự cần thiết phải xử lý và thao tác chuỗi một cách hiệu quả xuất phát từ tập trung chính của Go vào các máy chủ mạng và bộ phân giải dữ liệu, nơi xử lý chuỗi là một tác vụ phổ biến.

Một khía cạnh đáng chú ý của các hàm này là chúng được triển khai dựa trên các rune (đại diện của Go cho một điểm mã Unicode). Thiết kế này cho phép chúng dễ dàng xử lý chuỗi chứa các ký tự nhiều byte, làm cho cách tiếp cận xử lý chuỗi của Go vừa robust vừa thân thiện với Unicode.

Mặc dù việc sử dụng trực tiếp `Trim` và `TrimFunc` để loại bỏ dấu nháy là tiện lợi và phù hợp với Go, cần lưu ý rằng cho những nhiệm vụ xử lý chuỗi phức tạp hơn (ví dụ, dấu nháy lồng nhau, dấu nháy được thoát), biểu thức chính quy (thông qua gói `regexp`) hoặc phân tích thủ công có thể cung cấp giải pháp tốt hơn. Tuy nhiên, những lựa chọn thay thế này đi kèm với độ phức tạp và những cân nhắc về hiệu suất cao hơn. Do đó, đối với việc loại bỏ dấu nháy đơn giản, các phương pháp được trình bày ở trên tạo ra một sự cân bằng tốt giữa sự đơn giản, hiệu suất và chức năng.
