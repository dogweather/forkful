---
title:                "Kiểm tra xem thư mục có tồn tại không"
aliases:
- vi/go/checking-if-a-directory-exists.md
date:                  2024-02-03T17:53:28.198582-07:00
model:                 gpt-4-0125-preview
simple_title:         "Kiểm tra xem thư mục có tồn tại không"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/go/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Việc kiểm tra xem một thư mục có tồn tại trong Go là rất quan trọng đối với các ứng dụng tương tác với hệ thống tập tin, nhằm tránh lỗi khi cố gắng truy cập hoặc chỉnh sửa thư mục. Thao tác này rất cần thiết cho các nhiệm vụ như đảm bảo các điều kiện tiên quyết cho các hoạt động tập tin, quản lý cấu hình, và triển khai phần mềm dựa trên các cấu trúc thư mục cụ thể.

## Làm thế nào:

Trong Go, gói `os` cung cấp các chức năng để tương tác với hệ điều hành, bao gồm kiểm tra nếu một thư mục tồn tại. Dưới đây là cách bạn có thể làm điều đó:

```go
package main

import (
    "fmt"
    "os"
)

// isDirExists kiểm tra nếu một thư mục tồn tại
func isDirExists(path string) bool {
    info, err := os.Stat(path)
    if os.IsNotExist(err) {
        return false
    }
    return info.IsDir()
}

func main() {
    dirPath := "/tmp/exampleDir"

    if isDirExists(dirPath) {
        fmt.Printf("Thư mục %s tồn tại.\n", dirPath)
    } else {
        fmt.Printf("Thư mục %s không tồn tại.\n", dirPath)
    }
}
```
Ví dụ đầu ra:

```
Thư mục /tmp/exampleDir tồn tại.
```
hoặc 

```
Thư mục /tmp/exampleDir không tồn tại.
```

Tùy thuộc vào việc `/tmp/exampleDir` có tồn tại hay không.

## Đào sâu

Hàm `os.Stat` trả về một giao diện `FileInfo` và một lỗi. Nếu lỗi là loại `os.ErrNotExist`, nó có nghĩa là thư mục không tồn tại. Nếu không có lỗi, chúng ta tiếp tục kiểm tra nếu đường dẫn thực sự ám chỉ một thư mục thông qua phương thức `IsDir()` từ giao diện `FileInfo`.

Phương pháp này nổi bật vì sự đơn giản và hiệu quả của nó, nhưng quan trọng là phải lưu ý rằng việc kiểm tra sự tồn tại của thư mục trước khi thực hiện các hoạt động như tạo hoặc viết có thể dẫn đến các điều kiện chạy song song trong môi trường đồng thời. Đối với nhiều trường hợp, đặc biệt trong các ứng dụng đồng thời, có thể an toàn hơn khi thử thực hiện hoạt động (ví dụ, tạo tập tin) và xử lý lỗi sau đó, thay vì kiểm tra trước.

Theo lịch sử, cách tiếp cận này đã phổ biến trong lập trình do lô-gic đơn giản của nó. Tuy nhiên, sự phát triển của các tính toán đa luồng và đồng thời đòi hỏi sự chuyển dịch về phía xử lý lỗi mạnh mẽ hơn và tránh kiểm tra điều kiện tiên quyết như này nếu có thể. Điều này không làm giảm bớt ích lợi của nó đối với các ứng dụng đơn luồng đơn giản hơn hoặc các kịch bản mà tại đó các điều kiện như vậy ít được quan tâm hơn.
