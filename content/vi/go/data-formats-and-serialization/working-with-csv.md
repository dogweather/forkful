---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:12:10.054378-07:00
description: "L\xE0m th\u1EBF n\xE0o: L\xE0m vi\u1EC7c v\u1EDBi c\xE1c t\u1EC7p CSV\
  \ trong Go r\u1EA5t d\u1EC5 d\xE0ng, nh\u1EDD v\xE0o th\u01B0 vi\u1EC7n ti\xEAu\
  \ chu\u1EA9n, `encoding/csv`. D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9t b\xE0i gi\u1EDB\
  i thi\u1EC7u v\u1EC1 \u0111\u1ECDc v\xE0 vi\u1EBFt c\xE1c\u2026"
lastmod: '2024-03-13T22:44:36.012555-06:00'
model: gpt-4-0125-preview
summary: "L\xE0m vi\u1EC7c v\u1EDBi c\xE1c t\u1EC7p CSV trong Go r\u1EA5t d\u1EC5\
  \ d\xE0ng, nh\u1EDD v\xE0o th\u01B0 vi\u1EC7n ti\xEAu chu\u1EA9n, `encoding/csv`."
title: "L\xE0m vi\u1EC7c v\u1EDBi CSV"
weight: 37
---

## Làm thế nào:
Làm việc với các tệp CSV trong Go rất dễ dàng, nhờ vào thư viện tiêu chuẩn, `encoding/csv`. Dưới đây là một bài giới thiệu về đọc và viết các tệp CSV.

### Đọc một Tệp CSV
Để đọc từ một tệp CSV, bạn trước tiên mở tệp sử dụng `os.Open`, sau đó tạo một đọc CSV mới với `csv.NewReader`.

```go
package main

import (
    "encoding/csv"
    "fmt"
    "os"
)

func main() {
    file, err := os.Open("data.csv")
    if err != nil {
        panic(err)
    }
    defer file.Close()

    reader := csv.NewReader(file)
    records, err := reader.ReadAll()
    if err != nil {
        panic(err)
    }

    for _, record := range records {
        fmt.Println(record)
    }
}
```

Đoạn mã này sẽ đọc tất cả các bản ghi từ `data.csv` và in chúng ra. Mỗi bản ghi là một mảng các trường.

### Viết vào một Tệp CSV
Để viết, bạn sử dụng `csv.NewWriter` và `writer.WriteAll` hoặc `writer.Write` để viết nhiều hoặc một bản ghi CSV tương ứng.

```go
package main

import (
    "encoding/csv"
    "os"
)

func main() {
    file, err := os.Create("output.csv")
    if err != nil {
        panic(err)
    }
    defer file.Close()

    writer := csv.NewWriter(file)
    defer writer.Flush()

    records := [][]string{
        {"Name", "Age", "City"},
        {"John Doe", "30", "New York"},
        {"Jane Doe", "27", "Los Angeles"},
    }

    if err := writer.WriteAll(records); err != nil {
        panic(err)
    }
}
```

Điều này sẽ tạo một tệp tên là `output.csv` với các bản ghi được cung cấp. Hãy nhớ luôn làm sạch (flush) trình viết để đảm bảo tất cả dữ liệu đệm được viết vào tệp.

## Sâu hơn
Gói `encoding/csv` của Go cung cấp hỗ trợ mạnh mẽ cho việc đọc và viết tệp CSV nhưng nó được thiết kế với sự đơn giản trong tâm trí, điều này có nghĩa là nó không xử lý các tình huống phức tạp hơn như tự động phát hiện dấu phân cách, đối phó với dấu nháy hoặc ngắt dòng nhúng trong các trường mà không cần xử lý thủ công.

Trong lịch sử, việc xử lý CSV trong các ngôn ngữ lập trình thường gặp khó khăn do những phức tạp này, nhưng thư viện tiêu chuẩn của Go đã trừu tượng hóa nhiều vấn đề này, cho phép các nhà phát triển làm việc với dữ liệu CSV một cách tương đối dễ dàng. Tuy nhiên, cho các thao tác CSV phức tạp hơn, việc sử dụng các thư viện bên thứ ba như `gocsv` hoặc tự xử lý việc phân tích có thể là cần thiết.

Một điểm đáng chú ý của gói `csv` của Go là hỗ trợ của nó cho việc chỉ định dấu phẩy (dấu phân cách) tùy chỉnh, điều này cho phép nó làm việc một cách liền mạch với các biến thể của tệp CSV, như các giá trị tách bằng tab (TSV). Tuy nhiên, khi xử lý các tệp CSV không đều hoặc không chuẩn, các lập trình viên Go có thể thấy mình cần phải mở rộng các thực hiện đọc hoặc viết csv hiện có.

Mặc dù khả năng xử lý CSV của Go mạnh mẽ cho các mục đích chung, cho các ứng dụng yêu cầu thao tác dữ liệu cường độ cao, như khoa học dữ liệu hoặc các nhiệm vụ biến đổi dữ liệu phức tạp, các lập trình viên có thể tìm đến các gói xử lý dữ liệu chuyên dụng hoặc thậm chí các ngôn ngữ khác phù hợp hơn với các nhiệm vụ này, như Python với thư viện `pandas` của nó. Tuy nhiên, cho các hoạt động đọc viết CSV đơn giản, thư viện tiêu chuẩn của Go nổi bật vì hiệu quả và sự đơn giản của nó.
