---
title:                "Viết một tệp văn bản"
date:                  2024-01-28T22:12:54.219884-07:00
model:                 gpt-4-0125-preview
simple_title:         "Viết một tệp văn bản"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/go/writing-a-text-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Làm gì và Tại sao?

Việc viết một tệp văn bản nghĩa là lưu trữ dữ liệu trong một tệp chứa văn bản, thường là định dạng dễ đọc cho con người như `.txt` hoặc `.csv`. Các lập trình viên viết tệp để lưu và duy trì dữ liệu, có thể được đọc bởi con người hoặc sử dụng bởi các chương trình khác sau này.

## Làm thế nào:

Dưới đây là cách bạn viết một chuỗi vào một tệp văn bản trong Go:

```Go
package main

import (
	"log"
	"os"
)

func main() {
	message := "Hello, Go!"

	file, err := os.Create("example.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	_, err = file.WriteString(message)
	if err != nil {
		log.Fatal(err)
	}

	log.Println("Ghi tệp thành công!")
}
```

Chạy nó. Nếu thành công, bạn sẽ không thấy lỗi nhưng `example.txt` được tạo ra.

## Sâu xa hơn

Việc ghi vào tệp văn bản trong Go sử dụng gói `os`, cung cấp một giao diện độc lập với hệ điều hành. Hàm `os.Create` tạo hoặc rút gọn một tệp. Phương thức `File.WriteString` là cách thẳng thắn để viết chuỗi.

Trong lịch sử, việc xử lý tệp văn bản đã phát triển từ thư viên `stdio.h` của C. Trong Go, sự đơn giản là chìa khóa; bạn làm ít hơn nhưng đạt được nhiều hơn, tránh những điều không cần thiết. Có những phương án thay thế như `ioutil.WriteFile` nhưng không được khuyến nghị cho các tệp lớn do không hiệu quả về mặt bộ nhớ. `bufio` cung cấp I/O có đệm, giảm bớt lời gọi hệ thống và cải thiện hiệu suất.

## Xem thêm

- Go by Example: Viết tệp: https://gobyexample.com/writing-files
- Tài liệu Go cho gói os: https://pkg.go.dev/os
- Gói `bufio` của Go: https://pkg.go.dev/bufio
