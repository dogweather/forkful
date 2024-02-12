---
title:                "Viết một tệp văn bản"
aliases:
- /vi/go/writing-a-text-file.md
date:                  2024-02-03T18:15:16.912452-07:00
model:                 gpt-4-0125-preview
simple_title:         "Viết một tệp văn bản"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/go/writing-a-text-file.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại sao?

Việc viết một tệp văn bản trong Go bao gồm việc tạo ra và ghi các chuỗi dữ liệu vào một tệp văn bản mới hoặc đã tồn tại. Các lập trình viên thực hiện điều này để lưu trữ dữ liệu, như nhật ký ứng dụng, cài đặt cấu hình, hoặc kết quả từ các nhiệm vụ xử lý dữ liệu, làm cho nó trở thành một kỹ năng cơ bản cho quản lý dữ liệu và báo cáo trong phát triển phần mềm.

## Cách thực hiện:

Trong Go, việc viết vào một tệp văn bản được xử lý bởi gói `os` và `io/ioutil` (cho các phiên bản Go <1.16) hoặc `os` và `io` cùng với gói `os` cho Go 1.16 trở lên, thể hiện triết lý về sự đơn giản và hiệu quả của Go. API mới hơn khuyến khích thực hành tốt hơn với việc xử lý lỗi đơn giản hơn. Hãy nghiên cứu cách tạo và viết vào một tệp văn bản sử dụng gói `os` của Go.

Đầu tiên, hãy đảm bảo môi trường Go của bạn đã được thiết lập và sẵn sàng. Sau đó, tạo một tệp `.go`, ví dụ, `writeText.go`, và mở nó trong trình soạn thảo văn bản hoặc IDE của bạn.

Dưới đây là một ví dụ đơn giản về việc viết một chuỗi vào một tệp có tên `example.txt`:

```go
package main

import (
    "os"
    "log"
)

func main() {
    content := []byte("Chào các độc giả của Wired!\n")

    // Tạo hoặc ghi đè tệp example.txt
    err := os.WriteFile("example.txt", content, 0644)
    if err != nil {
        log.Fatal(err)
    }
}
```

Khi bạn chạy mã này bằng `go run writeText.go`, nó sẽ tạo ra (hoặc ghi đè nếu nó đã tồn tại) một tệp có tên `example.txt` với nội dung "Chào các độc giả của Wired!".

### Thêm vào Tệp

Nếu bạn muốn thêm nội dung thì sao? Go cũng cung cấp một cách linh hoạt để xử lý điều này:

```go
file, err := os.OpenFile("example.txt", os.O_APPEND|os.O_WRONLY|os.O_CREATE, 0644)
if err != nil {
    log.Fatal(err)
}
defer file.Close()

if _, err := file.WriteString("Thêm thêm văn bản.\n"); err != nil {
    log.Fatal(err)
}
```

Đoạn mã này mở `example.txt` ở chế độ thêm vào, viết một dòng thêm, và đảm bảo rằng tệp được đóng một cách đúng đắn ngay cả khi có lỗi xảy ra.

## Nghiên cứu sâu

Sự phát triển trong cách Go xử lý tệp phản ánh cam kết rộng lớn hơn của nó đối với sự đơn giản và hiệu quả trong mã. Các phiên bản đầu dựa nhiều hơn vào gói `ioutil`, yêu cầu nhiều từ ngữ hơn và khả năng gặp lỗi cao hơn một chút. Sự chuyển hướng nhằm cải thiện chức năng trong gói `os` và `io`, đặc biệt từ phiên bản 1.16 trở đi, minh họa những bước đi chủ động của Go nhằm đơn giản hóa các thao tác với tệp, khuyến khích việc xử lý lỗi một cách nhất quán, và làm cho ngôn ngữ trở nên dễ tiếp cận hơn.

Mặc dù thư viện đi kèm của Go đủ tốt cho nhiều trường hợp sử dụng, có những tình huống nơi các gói khác hoặc thư viện bên ngoài có thể được ưu tiên, đặc biệt là cho các thao tác với tệp phức tạp hơn hoặc khi làm việc trong các khuôn khổ lớn hơn cung cấp các trừu tượng riêng của chúng cho việc xử lý tệp. Tuy nhiên, cho các nhiệm vụ viết tệp trực tiếp, đơn giản, thư viện chuẩn thường cung cấp con đường hiệu quả và đúng đắn nhất trong lập trình Go. Sự chuyển dịch về các API đơn giản hóa, tổng hợp hơn cho các thao tác với tệp không chỉ làm cho mã Go dễ viết và bảo trì hơn mà còn củng cố triết lý về sự đơn giản, dễ đọc và thực tiễn của ngôn ngữ.
