---
title:                "Làm việc với số phức"
date:                  2024-01-28T22:12:55.504233-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với số phức"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/go/working-with-complex-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì và Tại sao?
Số phức, được tạo thành từ một phần thực và một phần ảo (như 5 + 7i), rất quan trọng trong các lĩnh vực như kỹ thuật, vật lý và xử lý tín hiệu. Lập trình viên làm việc với chúng để giải quyết các vấn đề trong những lĩnh vực này mà sẽ khó khăn để giải quyết chỉ với các số thực.

## Làm thế nào:
Go có hỗ trợ tích hợp sẵn cho số phức. Đây là một hướng dẫn nhanh:

```go
package main

import (
	"fmt"
	"math/cmplx"
)

func main() {
	// Tạo số phức
	a := complex(2, 3)
	b := 4 + 5i

	// Các phép toán cơ bản
	fmt.Println("Cộng:", a+b)
	fmt.Println("Trừ:", a-b)
	fmt.Println("Nhân:", a*b)
	fmt.Println("Chia:", a/b)

	// Các tính chất của số phức
	fmt.Println("Phần thực:", real(b))
	fmt.Println("Phần ảo:", imag(b))
	fmt.Println("Liên hợp:", cmplx.Conj(b))
	fmt.Println("Độ lớn:", cmplx.Abs(b))
	fmt.Println("Góc pha (radians):", cmplx.Phase(b))
}

```

Kết quả mẫu:

```
Cộng: (6+8i)
Trừ: (-2-2i)
Nhân: (-7+22i)
Chia: (0.5609756097560976+0.0487804878048781i)
Phần thực: 4
Phần ảo: 5
Liên hợp: (4-5i)
Độ lớn: 6.4031242374328485
Góc pha (radians): 0.8960553845713439
```

## Tìm hiểu sâu hơn
Ngày xưa, số phức bị xem với sự nghi ngờ - một số người cho rằng chúng vô dụng! Theo thời gian, sức mạnh của chúng trong mô tả các hiện tượng vật lý trở nên rõ ràng. Chúng là cơ bản trong vật lý lượng tử, lý thuyết điều khiển, và kỹ thuật điện, chỉ để kể tên một vài lĩnh vực.

Trong Go, số phức được biểu diễn bằng kiểu dữ liệu gọi là `complex128` (64 bit cho phần thực và ảo mỗi phần) hoặc `complex64` (32 bit mỗi phần). Về bản chất, đây thực sự chỉ là hai `float64`s hoặc `float32`s được ghép lại với nhau. Thư viện chuẩn của Go, `math/cmplx`, cung cấp các hàm cho các phép toán số phức. Điều này giúp bạn tránh khỏi việc phải đối mặt với những toán học phức tạp và cho phép bạn tập trung vào việc giải quyết vấn đề.

Các phương án thay thế cho việc hỗ trợ tích hợp sẵn của Go bao gồm việc sử dụng thư viện bên ngoài hoặc tự xây dựng cách xử lý số phức. Nhưng những phương án này hiếm khi cần thiết vì hỗ trợ bản địa của Go hiệu quả và được tích hợp tốt vào ngôn ngữ.

## Xem Thêm
Hãy kiểm tra những liên kết này để biết thêm về khả năng xử lý số phức của Go:
- Tài liệu chính thức của Go: https://golang.org/pkg/math/cmplx/
- Bài ôn lại toán học sâu hơn về số phức: https://www.mathsisfun.com/numbers/complex-numbers.html
- Các ứng dụng thực tiễn của số phức trong kỹ thuật: https://ieeexplore.ieee.org/document/528dunno
