---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:06:25.690416-07:00
description: "L\xE0m th\u1EBF n\xE0o: \u0110\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3\
  n trong Go c\xF3 th\u1EC3 \u0111\u01B0\u1EE3c th\u1EF1c hi\u1EC7n theo m\u1ED9t\
  \ s\u1ED1 c\xE1ch, nh\u01B0ng m\u1ED9t trong nh\u1EEFng ph\u01B0\u01A1ng ph\xE1\
  p \u0111\u01A1n gi\u1EA3n nh\u1EA5t l\xE0 s\u1EED d\u1EE5ng g\xF3i `ioutil`.\u2026"
lastmod: '2024-03-13T22:44:36.005950-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n trong Go c\xF3 th\u1EC3\
  \ \u0111\u01B0\u1EE3c th\u1EF1c hi\u1EC7n theo m\u1ED9t s\u1ED1 c\xE1ch, nh\u01B0\
  ng m\u1ED9t trong nh\u1EEFng ph\u01B0\u01A1ng ph\xE1p \u0111\u01A1n gi\u1EA3n nh\u1EA5\
  t l\xE0 s\u1EED d\u1EE5ng g\xF3i `ioutil`."
title: "\u0110\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n"
weight: 22
---

## Làm thế nào:
Đọc một tệp văn bản trong Go có thể được thực hiện theo một số cách, nhưng một trong những phương pháp đơn giản nhất là sử dụng gói `ioutil`. Dưới đây là một ví dụ cơ bản:

```go
package main

import (
    "fmt"
    "io/ioutil"
    "log"
)

func main() {
    content, err := ioutil.ReadFile("example.txt")
    if err != nil {
        log.Fatal(err)
    }

    fmt.Println(string(content))
}
```

Giả sử `example.txt` chứa "Hello, Go!", chương trình này sẽ xuất ra:

```
Hello, Go!
```

Tuy nhiên, kể từ Go 1.16, gói `ioutil` đã bị khai tử, và được khuyến nghị sử dụng các gói `os` và `io` thay thế. Dưới đây là cách bạn có thể thực hiện cùng một công việc với các gói này:

```go
package main

import (
    "bufio"
    "fmt"
    "log"
    "os"
)

func main() {
    file, err := os.Open("example.txt")
    if err != nil {
        log.Fatal(err)
    }
    defer file.Close()

    scanner := bufio.NewScanner(file)
    while scanner.Scan() {
        fmt.Println(scanner.Text())
    }

    if err := scanner.Err(); err != nil {
        log.Fatal(err)
    }
}
```

Phương pháp này không chỉ hiện đại hơn mà còn hỗ trợ các tệp lớn hơn, vì nó đọc tệp từng dòng một thay vì nạp toàn bộ nội dung vào bộ nhớ cùng một lần.

## Đào sâu:
Cách Go xử lý các thao tác với tệp, bao gồm cả đọc từ các tệp, phản ánh triết lý của ngôn ngữ về sự đơn giản và hiệu quả. Ban đầu, gói `ioutil` cung cấp các thao tác với tệp một cách trực tiếp. Tuy nhiên, với sự cải thiện trong thư viện chuẩn của Go và sự chuyển đổi hướng tới xử lý lỗi một cách rõ ràng hơn và quản lý tài nguyên, các gói `os` và `io` đã trở thành lựa chọn ưu tiên để làm việc với tệp.

Những thay đổi này nhấn mạnh cam kết của Go với hiệu suất và an toàn, đặc biệt là trong việc tránh các vấn đề về bộ nhớ có thể phát sinh từ việc nạp các tệp lớn hoàn toàn. Phương thức `bufio.Scanner` được giới thiệu để đọc các tệp từng dòng một làm nổi bật khả năng thích nghi và tập trung vào các thách thức tính toán hiện đại của ngôn ngữ, chẳng hạn như xử lý các bộ dữ liệu lớn hoặc dữ liệu đang phát.

Mặc dù có các thư viện bên ngoài có sẵn để làm việc với các tệp trong Go, khả năng của thư viện chuẩn thường được ưu tiên và đủ dùng cho sự ổn định và hiệu suất của nó. Điều này đảm bảo rằng các nhà phát triển Go có thể quản lý các thao tác với tệp một cách hiệu quả mà không cần phụ thuộc vào các thành phần bổ sung, phù hợp với tinh thần tối giản tổng thể và thiết kế của ngôn ngữ để xây dựng phần mềm hiệu quả, đáng tin cậy.
