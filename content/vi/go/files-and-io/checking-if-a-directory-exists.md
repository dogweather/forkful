---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:53:28.198582-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong Go, g\xF3i `os` cung c\u1EA5p c\xE1c ch\u1EE9\
  c n\u0103ng \u0111\u1EC3 t\u01B0\u01A1ng t\xE1c v\u1EDBi h\u1EC7 \u0111i\u1EC1u\
  \ h\xE0nh, bao g\u1ED3m ki\u1EC3m tra n\u1EBFu m\u1ED9t th\u01B0 m\u1EE5c t\u1ED3\
  n t\u1EA1i. D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch b\u1EA1n c\xF3\u2026"
lastmod: '2024-03-13T22:44:36.002017-06:00'
model: gpt-4-0125-preview
summary: "Trong Go, g\xF3i `os` cung c\u1EA5p c\xE1c ch\u1EE9c n\u0103ng \u0111\u1EC3\
  \ t\u01B0\u01A1ng t\xE1c v\u1EDBi h\u1EC7 \u0111i\u1EC1u h\xE0nh, bao g\u1ED3m ki\u1EC3\
  m tra n\u1EBFu m\u1ED9t th\u01B0 m\u1EE5c t\u1ED3n t\u1EA1i."
title: "Ki\u1EC3m tra xem th\u01B0 m\u1EE5c c\xF3 t\u1ED3n t\u1EA1i kh\xF4ng"
weight: 20
---

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
