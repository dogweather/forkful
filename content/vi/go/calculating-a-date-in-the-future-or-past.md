---
title:                "Tính toán ngày trong tương lai hoặc quá khứ"
date:                  2024-01-28T21:55:58.624383-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tính toán ngày trong tương lai hoặc quá khứ"

category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/go/calculating-a-date-in-the-future-or-past.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Lý do & Tại sao?

Tính toán một ngày trong tương lai hoặc quá khứ đúng như nó nghe có vẻ - xác định xem ngày nào sẽ là, ví dụ, 10 ngày từ bây giờ, hoặc ngày nào đã là 50 ngày trước. Lập trình viên làm điều này cho những việc như đặt thời hạn, ngày hết hạn, hoặc xử lý đặt chỗ.

## Cách thực hiện:

Hãy thao tác với thời gian trong Go:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	// Ngày hôm nay
	today := time.Now()
	fmt.Println("Hôm nay là:", today.Format("Jan 2, 2006"))

	// Ngày trong tương lai, 10 ngày từ bây giờ
	future := today.Add(10 * 24 * time.Hour)
	fmt.Println("10 ngày từ bây giờ:", future.Format("Jan 2, 2006"))

	// Ngày trong quá khứ, 50 ngày trước
	past := today.Add(-50 * 24 * time.Hour)
	fmt.Println("50 ngày trước:", past.Format("Jan 2, 2006"))
}
```

Chạy nó và bạn sẽ thấy điều gì đó như:

```
Hôm nay là: Mar 15, 2023
10 ngày từ bây giờ: Mar 25, 2023
50 ngày trước: Jan 24, 2023
```

## Khám phá sâu hơn

Tại sao lại quan tâm đến ngày tháng? Chà, từ lịch sử, việc theo dõi thời gian đã là chìa khóa cho nông nghiệp, khoa học, lịch sử, bạn tên nó. Trong lĩnh vực máy tính, nó cũng quan trọng không kém - nghĩ về các nhiệm vụ như sao lưu, kiểm tra hết hạn, và lập lịch trình.

Trước khi có gói `time` của Go, chúng ta phải dựa vào những thư viện kém trực quan hơn hoặc, trời ơi, tính toán thủ công. Bây giờ, chúng ta có thể thao tác với ngày tháng bằng cách sử dụng `Add` để thêm thời lượng, hoặc `Sub` để tìm thời lượng giữa hai ngày.

Cũng, đây là một sự thật thú vị: các phép tính xem xét năm nhuận và thứ khác, nhưng không có xử lý cho những điều kỳ quặc trong lịch do con người tạo ra (như khi nước Anh bỏ qua 11 ngày năm 1752).

Có phương án khác không? Chắc chắn rồi. Bạn có thể sử dụng `AddDate` để thêm số cụ thể của năm, tháng, và ngày, nếu bạn không thích cách tiếp cận `duration * time.Hour`.

Về mặt triển khai, Go sử dụng lịch Gregory theo lịch sử, được mở rộng trở lại đến năm đầu tiên và tiến lên tương lai xa. Đó là cùng một hệ thống chúng ta sử dụng hàng ngày, trừ những khúc mắc của các cải cách lịch sử.

## Xem thêm

- Đặc tả Ngôn ngữ Lập trình Go về thời gian: https://golang.org/ref/spec#Time
- Tài liệu gói `time` của Go: https://pkg.go.dev/time
- Bài nói của Rob Pike về Định dạng Thời gian của Go: https://www.youtube.com/watch?v=rKnDgT73v8s
