---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:09:53.510219-07:00
description: "L\xE0m th\u1EBF n\xE0o: Tr\u01B0\u1EDBc ti\xEAn, \u0111\u1EA3m b\u1EA3\
  o b\u1EA1n \u0111\xE3 c\xE0i \u0111\u1EB7t Go b\u1EB1ng c\xE1ch ch\u1EA1y `go version`\
  \ tr\xEAn terminal c\u1EE7a b\u1EA1n. B\u1EA1n s\u1EBD th\u1EA5y phi\xEAn b\u1EA3\
  n Go b\u1EA1n \u0111\xE3 c\xE0i \u0111\u1EB7t \u0111\u01B0\u1EE3c xu\u1EA5t\u2026"
lastmod: '2024-03-13T22:44:35.983021-06:00'
model: gpt-4-0125-preview
summary: "Tr\u01B0\u1EDBc ti\xEAn, \u0111\u1EA3m b\u1EA3o b\u1EA1n \u0111\xE3 c\xE0\
  i \u0111\u1EB7t Go b\u1EB1ng c\xE1ch ch\u1EA1y `go version` tr\xEAn terminal c\u1EE7\
  a b\u1EA1n."
title: "Kh\u1EDFi \u0111\u1EA7u m\u1ED9t d\u1EF1 \xE1n m\u1EDBi"
weight: 1
---

## Làm thế nào:
Trước tiên, đảm bảo bạn đã cài đặt Go bằng cách chạy `go version` trên terminal của bạn. Bạn sẽ thấy phiên bản Go bạn đã cài đặt được xuất ra. Tiếp theo, hãy bắt đầu một dự án mới. Di chuyển đến không gian làm việc của bạn và chạy:

```shell
mkdir hello-world
cd hello-world
```

Điều này tạo và di chuyển bạn vào thư mục mới cho dự án của bạn. Bây giờ, khởi tạo module:

```shell
go mod init example.com/hello-world
```

Thay thế `example.com/hello-world` bằng đường dẫn module của bạn. Lệnh này tạo một tệp `go.mod` trong thư mục của bạn, đánh dấu sự bắt đầu của một module Go mới. Dưới đây là cách `go.mod` có thể trông như thế nào:

```plaintext
module example.com/hello-world

go 1.18
```

`go.mod` theo dõi các phụ thuộc của dự án bạn. Bây giờ, tạo một tệp `main.go`:

```shell
touch main.go
```

Mở `main.go` trong trình soạn thảo ưa thích của bạn và thêm mã sau để in "Hello, World!":

```go
package main

import "fmt"

func main() {
    fmt.Println("Hello, World!")
}
```

Để chạy chương trình của bạn, quay lại terminal và thực thi:

```shell
go run main.go
```

Bạn sẽ thấy:

```plaintext
Hello, World!
```

Chúc mừng! Bạn vừa bắt đầu một dự án Go mới và chạy chương trình Go đầu tiên của mình.

## Sâu hơn nữa
Sáng kiến ​​giới thiệu modules như là tiêu chuẩn quản lý phụ thuộc trong Go là một bước chuyển mình đáng kể trong hệ sinh thái Go, chính thức được chấp nhận trong Go 1.11. Trước modules, các nhà phát triển Go dựa vào biến môi trường GOPATH để quản lý phụ thuộc, điều này kém trực quan và thường dẫn đến "địa ngục phụ thuộc" nổi tiếng.

Modules cung cấp một cách đóng gói để quản lý phụ thuộc dự án, phiên bản, và là một bước tiến hướng tới việc làm cho các dự án Go trở nên tự chứa và di động hơn. Mỗi module chỉ rõ các phụ thuộc của nó mà Go theo dõi trong tệp `go.mod`, đơn giản hóa quản lý phụ thuộc qua các môi trường và giai đoạn phát triển khác nhau.

Tuy nhiên, đáng chú ý là trong khi các module Go hiện là tiêu chuẩn, một số dự án cũ có thể vẫn sử dụng GOPATH. Đối với hầu hết các dự án mới, modules cung cấp một hệ thống quản lý đơn giản và hiệu quả hơn, nhưng hiểu về GOPATH có thể hữu ích cho việc duy trì hoặc đóng góp vào các cơ sở mã Go cũ.

Về các phương án khác, trong khi các module Go hiện là tiêu chuẩn thực tế, cộng đồng Go đã thử nghiệm với các công cụ quản lý phụ thuộc khác như `dep` trong quá khứ. Tuy nhiên, những công cụ này chủ yếu đã được thay thế bằng hỗ trợ module chính thức được tích hợp vào công cụ Go.
