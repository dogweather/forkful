---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:58:03.020387-07:00
description: "Vi\u1EC7c t\u1EA1o ra c\xE1c s\u1ED1 ng\u1EABu nhi\xEAn trong l\u1EAD\
  p tr\xECnh l\xE0 vi\u1EC7c t\u1EA1o ra m\u1ED9t chu\u1ED7i s\u1ED1 m\xE0 kh\xF4\
  ng th\u1EC3 d\u1EF1 \u0111o\xE1n m\u1ED9t c\xE1ch h\u1EE3p l\xFD t\u1ED1t h\u01A1\
  n l\xE0 qua c\u01A1 h\u1ED9i. C\xE1c l\u1EADp tr\xECnh vi\xEAn\u2026"
lastmod: '2024-03-13T22:44:35.976446-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c t\u1EA1o ra c\xE1c s\u1ED1 ng\u1EABu nhi\xEAn trong l\u1EADp tr\xEC\
  nh l\xE0 vi\u1EC7c t\u1EA1o ra m\u1ED9t chu\u1ED7i s\u1ED1 m\xE0 kh\xF4ng th\u1EC3\
  \ d\u1EF1 \u0111o\xE1n m\u1ED9t c\xE1ch h\u1EE3p l\xFD t\u1ED1t h\u01A1n l\xE0 qua\
  \ c\u01A1 h\u1ED9i."
title: "Sinh s\u1ED1 ng\u1EABu nhi\xEAn"
weight: 12
---

## Gì & Tại sao?

Việc tạo ra các số ngẫu nhiên trong lập trình là việc tạo ra một chuỗi số mà không thể dự đoán một cách hợp lý tốt hơn là qua cơ hội. Các lập trình viên thực hiện điều này vì nhiều lý do, bao gồm mô phỏng, trò chơi và các ứng dụng bảo mật, nơi sự không thể dự đoán là chìa khóa cho chức năng hoặc bí mật.

## Làm thế nào:

Trong Go, các số ngẫu nhiên được tạo ra bằng cách sử dụng gói `math/rand` cho các số giả ngẫu nhiên hoặc `crypto/rand` cho các số giả ngẫu nhiên an toàn mật mã. Hãy khám phá cả hai.

### Sử dụng `math/rand` cho Các Số Giả Ngẫu Nhiên

Đầu tiên, nhập gói `math/rand` và gói `time` để gieo hạt cho máy sinh số. Việc gieo hạt đảm bảo rằng bạn nhận được một chuỗi số khác nhau mỗi lần chạy.

```go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	rand.Seed(time.Now().UnixNano())
	fmt.Println("Một số ngẫu nhiên:", rand.Intn(100)) // Tạo ra một số giữa 0 và 99
}
```

Ví dụ về đầu ra: `Một số ngẫu nhiên: 42`

### Sử dụng `crypto/rand` cho Các Số Giả Ngẫu Nhiên An Toàn Mật Mã

Đối với các ứng dụng nhạy cảm với an ninh hơn, gói `crypto/rand` phù hợp vì nó sinh ra các số ngẫu nhiên khó dự đoán, làm cho chúng phù hợp cho các hoạt động mật mã.

```go
package main

import (
	"crypto/rand"
	"fmt"
	"math/big"
)

func main() {
	n, _ := rand.Int(rand.Reader, big.NewInt(100))
	fmt.Println("Một số ngẫu nhiên an toàn:", n)
}
```

Ví dụ về đầu ra: `Một số ngẫu nhiên an toàn: 81`

## Đi Sâu

Sự khác biệt cơ bản giữa gói `math/rand` và `crypto/rand` trong Go xuất phát từ nguồn entropy của chúng và các trường hợp sử dụng dự định của chúng. `math/rand` sinh ra các số giả ngẫu nhiên dựa trên hạt giống ban đầu; do đó, chuỗi là xác định và có thể dự đoán nếu hạt giống được biết. Điều này phù hợp cho các tình huống nơi hiệu suất cao và không phải sự không thể dự đoán tuyệt đối là mối quan tâm chính, như mô phỏng hoặc trò chơi.

Ngược lại, `crypto/rand` lấy sự ngẫu nhiên từ hệ điều hành cơ bản, làm cho nó phù hợp cho các sử dụng mật mã nơi sự không thể dự đoán là quan trọng. Tuy nhiên, điều này đi kèm với chi phí về hiệu suất và độ phức tạp trong việc xử lý các số nó sinh ra (như xử lý kiểu `*big.Int` cho các số nguyên).

Một cách lịch sử, ý tưởng về việc tạo số ngẫu nhiên trong máy tính luôn luôn diễn ra trên bờ vực của "ngẫu nhiên" thực sự, với các hệ thống sơ khai phụ thuộc nhiều vào các thuật toán xác định mô phỏng sự ngẫu nhiên. Khi máy tính phát triển, các thuật toán này cũng vậy, kết hợp thêm nhiều nguồn entropy phức tạp hơn từ môi trường của chúng.

Dẫu cho có những tiến bộ, cuộc đấu tranh cho sự ngẫu nhiên hoàn hảo trong tính toán về cơ bản là một nghịch lý, do bản chất xác định của chính máy tính. Đó là lý do tại sao, cho hầu hết các ứng dụng nơi sự dự đoán có thể là hại, các số giả ngẫu nhiên an toàn mật mã từ các nguồn như `crypto/rand` là lựa chọn tốt hơn, bất chấp chi phí phát sinh.

Về bản chất, cách tiếp cận của Go với hai gói phân biệt cho việc tạo số ngẫu nhiên một cách tinh tế giải quyết sự cân nhắc giữa hiệu suất và an ninh, cho phép các nhà phát triển chọn dựa trên nhu cầu cụ thể của họ.
