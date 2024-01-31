---
title:                "Sinh số ngẫu nhiên"
date:                  2024-01-28T22:00:59.141093-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sinh số ngẫu nhiên"

category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/go/generating-random-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì và Tại sao?

Việc tạo số ngẫu nhiên trong Go bao gồm việc sử dụng gói `math/rand` để tạo ra các số ngẫu nhiên giả lập cho các ứng dụng như mô phỏng thí nghiệm, tạo dữ liệu thử nghiệm, hoặc thêm tính không dự đoán vào trò chơi. Các lập trình viên sử dụng tính năng này để tạo ra hành vi phần mềm động và ít dự đoán hơn.

## Làm thế nào:

Để bắt đầu tạo số ngẫu nhiên trong Go, bạn cần phải nhập gói `math/rand` và gói `time` để gieo số cho bộ sinh số ngẫu nhiên thêm tính không dự đoán. Dưới đây là một ví dụ cơ bản:

```Go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	// Gieo số cho bộ sinh
	rand.Seed(time.Now().UnixNano())
	
	// Sinh một số nguyên ngẫu nhiên từ 0 đến 99
	randomInt := rand.Intn(100)
	fmt.Println("Số Nguyên Ngẫu Nhiên:", randomInt)
	
	// Sinh một số thực ngẫu nhiên từ 0.0 đến 1.0
	randomFloat := rand.Float64()
	fmt.Println("Số Thực Ngẫu Nhiên:", randomFloat)
}
```

Kết quả ví dụ có thể là:

```
Số Nguyên Ngẫu Nhiên: 42
Số Thực Ngẫu Nhiên: 0.7304601899194229
```

Hãy nhớ rằng, mỗi lần thực thi sẽ sản xuất ra những con số khác nhau do được gieo số bằng thời gian hiện tại.

## Đào sâu

Gói `math/rand` trong Go hiện thực hóa các bộ sinh số ngẫu nhiên giả lập (PRNGs) cho các phân phối khác nhau. Mặc dù rất hiệu quả cho nhiều ứng dụng, điều quan trọng cần lưu ý là các số được tạo bởi `math/rand` không phù hợp để sử dụng cho mục đích mật mã học do bản chất xác định của chúng. Đối với các nhu cầu mật mã học, gói `crypto/rand` là lựa chọn phù hợp, cung cấp một bộ sinh số ngẫu nhiên an toàn.

Sự hiện thực hóa của `math/rand` dựa trên một thuật toán bộ sinh số ngẫu nhiên loại trừ, hiệu quả và có một chu kỳ tương đối dài trước khi lặp lại các dãy số. Tuy nhiên, đối với các ứng dụng yêu cầu các chuỗi thực sự ngẫu nhiên, như hoạt động mật mã học, các bộ sinh số ngẫu nhiên phần cứng (RNGs) hoặc gói `crypto/rand`, giao tiếp với các nguồn ngẫu nhiên an toàn cụ thể của hệ thống, được khuyến khích sử dụng.

`math/rand` cho phép gieo số để giới thiệu biến thể, nhưng cùng một gieo số luôn tạo ra cùng một dãy số, làm nổi bật bản chất xác định của tính ngẫu nhiên của nó. Điều này làm cho nó phù hợp với mô phỏng hoặc trò chơi nơi có thể mong muốn tính khả tái tạo cho mục đích gỡ lỗi hoặc thử nghiệm.
