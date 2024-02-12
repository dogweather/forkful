---
title:                "Làm việc với số phức"
aliases:
- /vi/go/working-with-complex-numbers/
date:                  2024-02-03T18:14:37.768197-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với số phức"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/go/working-with-complex-numbers.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Lý do & Tại sao?

Làm việc với số phức trong lập trình đề cập đến việc thao tác với các số có cả phần thực và phần ảo, thường được biểu diễn như `a + bi`. Các lập trình viên giải quyết số phức trong nhiều lĩnh vực, như kỹ thuật, vật lý và phân tích dữ liệu, để giải quyết các vấn đề liên quan đến căn bậc hai của số âm, phân tích hình dạng sóng, và nhiều hơn nữa.

## Làm thế nào:

Trong Go, số phức được xử lý sử dụng các hàm built-in `complex`, `real`, và `imag`, cùng với các kiểu `complex64` và `complex128` (đại diện cho số phức 64-bit và 128-bit tương ứng). Dưới đây là hướng dẫn nhanh:

```go
package main

import (
	"fmt"
)

func main() {
	// Tạo số phức
	a := complex(2, 3) // 2+3i
	b := complex(1, -1) // 1-1i

	// Các phép toán số học
	c := a + b
	fmt.Println("Phép cộng:", c) // Đầu ra: Phép cộng: (3+2i)

	d := a * b
	fmt.Println("Phép nhân:", d) // Đầu ra: Phép nhân: (5+1i)

	// Truy cập phần thực và phần ảo
	phanThuc := real(a)
	phanAo := imag(a)
	fmt.Printf("Phần thực: %.1f, Phần ảo: %.1f\n", phanThuc, phanAo) // Đầu ra: Phần thực: 2.0, Phần ảo: 3.0

	// Liên hợp và độ lớn có thể được tính toán
	lienHop := complex(real(a), -imag(a)) // Thủ công
	fmt.Println("Liên hợp của a:", lienHop) // Đầu ra: Liên hợp của a: (2-3i)
}

```

Ví dụ này chỉ bao gồm cơ bản, nhưng có nhiều thứ bạn có thể làm với số phức, bao gồm việc tận dụng gói `math/cmplx` cho các thao tác nâng cao hơn như tìm độ lớn, pha, và nhiều hơn nữa.

## Sâu hơn nữa

Khái niệm của số phức có từ thế kỷ 16, nhưng chỉ được công nhận rộng rãi và hệ thống hoá một cách nghiêm ngặt vào thế kỷ 19. Trong lập trình máy tính, số phức đã là một phần không thể thiếu cho số học phức tạp trong các tính toán khoa học và kỹ thuật từ những ngày đầu. Cách tiếp cận của Go đối với số phức, bằng cách biến chúng thành một công dân hạng nhất với sự hỗ trợ built-in và hỗ trợ thư viện tiêu chuẩn toàn diện thông qua gói `math/cmplx`, nổi bật giữa các ngôn ngữ lập trình. Quyết định thiết kế này phản ánh sự nhấn mạnh của Go vào sự đơn giản và hiệu suất.

Tuy nhiên, đáng chú ý là làm việc với số phức trong Go, mặc dù mạnh mẽ, có thể không luôn luôn là cách tiếp cận tốt nhất cho tất cả các ứng dụng, đặc biệt là những ứng dụng yêu cầu toán học biểu tượng hoặc số học chính xác cao. Ngôn ngữ và môi trường chuyên biệt trong tính toán khoa học, như Python với các thư viện như NumPy và SciPy, hoặc phần mềm như MATLAB, có thể cung cấp nhiều tính linh hoạt hơn và một loạt chức năng rộng lớn hơn cho các ứng dụng cụ thể.

Dẫu vậy, đối với lập trình hệ thống và các ngữ cảnh nơi tích hợp các phép toán số phức vào một ứng dụng lớn hơn, nhạy cảm với hiệu suất là rất quan trọng, sự hỗ trợ bản địa của Go cho số phức cung cấp một lựa chọn hiệu quả độc đáo.
