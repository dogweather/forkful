---
title:                "Làm tròn số"
date:                  2024-01-28T22:07:23.230027-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm tròn số"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/go/rounding-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Làm tròn số có nghĩa là điều chỉnh một số về gần giá trị nguyên hoặc vị trí thập phân được chỉ định nhất. Việc này được thực hiện để đơn giản hóa giá trị, làm cho chúng dễ đọc hơn, hoặc phù hợp với một số ràng buộc nhất định, như khi làm việc với tiền tệ.

## Làm thế nào:
Gói `math` của Go là bạn đồng hành của bạn cho việc làm tròn. Sử dụng `math.Round`, `math.Floor`, và `math.Ceil` để đơn giản hóa:

```go
package main

import (
	"fmt"
	"math"
)

func main() {
	number := 3.14159
	fmt.Println("Round:", math.Round(number))  // Làm tròn về số nguyên gần nhất
	fmt.Println("Floor:", math.Floor(number)) // Làm tròn xuống
	fmt.Println("Ceil: ", math.Ceil(number))  // Làm tròn lên
}
```

Kết quả mẫu:
```
Round: 3
Floor: 3
Ceil: 4
```

Đối với vị trí thập phân cụ thể, nhân, làm tròn, sau đó chia:

```go
func roundToDecimalPlace(number float64, decimalPlaces int) float64 {
	shift := math.Pow(10, float64(decimalPlaces))
	return math.Round(number*shift) / shift
}

func main() {
	number := 3.14159
	fmt.Println("Làm tròn tới 2 vị trí thập phân:", roundToDecimalPlace(number, 2))
}
```

Kết quả mẫu:
```
Làm tròn tới 2 vị trí thập phân: 3.14
```

## Tìm hiểu sâu
Việc làm tròn số không phải là mới—nó đã có từ thời cổ đại, luôn hướng tới sự đơn giản. Phương thức `math.Round` trong Go sử dụng [làm tròn của ngân hàng](https://en.wikipedia.org/wiki/Rounding#Round_half_to_even), có nghĩa là 0,5 được làm tròn về số chẵn gần nhất, giảm thiểu sự thiên vị có thể ảnh hưởng đến tổng.

Số dấu phẩy động có thể gây khó khăn do biểu diễn nhị phân của chúng, có thể không chính xác biểu diễn tất cả các số thập phân. Tuy nhiên, cách tiếp cận của Go, phần lớn thời gian, vẫn duy trì hành vi mong đợi.

Các phương thức làm tròn khác tồn tại, như "làm tròn đến một nửa lên" hoặc "làm tròn một nửa ra xa từ số không", nhưng thư viện chuẩn của Go là những gì sẵn có ngay. Đối với nhu cầu phức tạp hơn, bạn có thể cần đến một thư viện bên thứ ba hoặc tự mình tạo ra giải pháp.

## Xem thêm
- Gói `math` của Go: [https://pkg.go.dev/math](https://pkg.go.dev/math)
- Tiêu chuẩn IEEE 754 cho số học dấu phẩy động (cơ sở của Go trong xử lý số dấu phẩy động): [https://ieeexplore.ieee.org/document/4610935](https://ieeexplore.ieee.org/document/4610935)
- Hiểu về số dấu phẩy động: ["Những điều mọi Khoa học Máy tính cần biết về Số học Dấu phẩy Động"](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html)
