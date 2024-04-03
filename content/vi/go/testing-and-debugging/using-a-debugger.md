---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:47.833278-07:00
description: "L\xE0m th\u1EBF n\xE0o: Go cung c\u1EA5p m\u1ED9t ti\u1EC7n \xEDch t\xED\
  ch h\u1EE3p cho vi\u1EC7c g\u1EE1 l\u1ED7i g\u1ECDi l\xE0 `delve`. \u0110\xE2y l\xE0\
  \ m\u1ED9t c\xF4ng c\u1EE5 g\u1EE1 l\u1ED7i \u0111\u1EA7y \u0111\u1EE7 t\xEDnh n\u0103\
  ng cho ph\xE9p b\u1EA1n th\u1EF1c hi\u1EC7n ch\u01B0\u01A1ng\u2026"
lastmod: '2024-03-13T22:44:35.988288-06:00'
model: gpt-4-0125-preview
summary: "Go cung c\u1EA5p m\u1ED9t ti\u1EC7n \xEDch t\xEDch h\u1EE3p cho vi\u1EC7\
  c g\u1EE1 l\u1ED7i g\u1ECDi l\xE0 `delve`."
title: "S\u1EED d\u1EE5ng tr\xECnh g\u1EE1 l\u1ED7i"
weight: 35
---

## Làm thế nào:
Go cung cấp một tiện ích tích hợp cho việc gỡ lỗi gọi là `delve`. Đây là một công cụ gỡ lỗi đầy đủ tính năng cho phép bạn thực hiện chương trình Go từng bước một, kiểm tra các biến trong chương trình và đánh giá các biểu thức.

Để bắt đầu, bạn cần phải cài đặt `delve` trước. Bạn có thể làm điều này bằng cách chạy:

```shell
go get -u github.com/go-delve/delve/cmd/dlv
```

Bây giờ, hãy debug một chương trình Go đơn giản. Xem xét một chương trình `main.go`:

```go
package main

import "fmt"

func main() {
    message := "Debugging in Go"
    fmt.Println(message)
}
```

Để bắt đầu gỡ lỗi chương trình này, mở một terminal trong thư mục dự án và thực thi:

```shell
dlv debug
```

Lệnh này biên dịch chương trình với tối ưu hóa bị vô hiệu hóa (để cải thiện trải nghiệm gỡ lỗi), khởi động nó và gắn một debugger vào nó.

Một khi `delve` đang chạy, bạn đang ở trong shell debugger tương tác. Dưới đây là một số lệnh cơ bản:

- `break main.main` thiết lập một điểm dừng tại hàm `main`.
- `continue` tiếp tục thực thi chương trình cho đến khi gặp một điểm dừng.
- `print message` sẽ in giá trị của biến `message`.
- `next` tiến chương trình thực thi đến dòng tiếp theo.
- `quit` thoát khỏi debugger.

Kết quả khi gặp điểm dừng và in biến có thể trông như sau:

```shell
Breakpoint 1 at 0x49ecf3 for main.main() ./main.go:6
> main.main() ./main.go:6 (hits goroutine(1):1 total:1) (PC: 0x49ecf3)
     1: package main
     2:
     3: import "fmt"
     4:
     5: func main() {
     6: =>    message := "Debugging in Go"
     7:       fmt.Println(message)
     8: }
(dlv) print message
"Debugging in Go"
```

Sử dụng những lệnh này, bạn có thể từng bước đi qua chương trình của mình, kiểm tra trạng thái của nó khi bạn thực hiện để hiểu cách nó hoạt động và xác định bất kỳ vấn đề nào.

## Sâu hơn nữa
Việc chọn `delve` là công cụ gỡ lỗi của sự lựa chọn cho Go thay vì các công cụ truyền thống như GDB (GNU Debugger) chủ yếu là do bản chất của mô hình thực thi và thời gian chạy của Go. GDB ban đầu không được thiết kế với thời gian chạy của Go trong tâm trí, khiến `delve` trở thành lựa chọn phù hợp hơn cho các nhà phát triển Go. `Delve` được thiết kế đặc biệt cho Go, cung cấp một trải nghiệm gỡ lỗi trực quan hơn cho các Go routine, kênh và các cấu trúc đặc thù của Go khác.

Hơn nữa, `delve` hỗ trợ một loạt các tính năng vượt trội so với các tính năng cơ bản của GDB khi làm việc với các chương trình Go. Điều này bao gồm nhưng không giới hạn ở: gắn vào các quy trình đang chạy để gỡ lỗi; các điểm dừng có điều kiện; và đánh giá các biểu thức phức tạp có thể liên quan đến các nguyên tắc đồng thời của Go.

Mặc dù `delve` là debugger mà nhiều nhà phát triển Go lựa chọn, đáng chú ý là bộ công cụ Go cũng bao gồm các hình thức hỗ trợ gỡ lỗi nhẹ hơn, chẳng hạn như công cụ `pprof` được tích hợp sẵn cho việc xuất dữ liệu và công cụ `trace` cho việc hiện thị đồng thời. Những công cụ này đôi khi có thể cung cấp một lộ trình nhanh hơn hoặc trên một cấp độ cao hơn cho việc chẩn đoán vấn đề hiệu suất chương trình hoặc lỗi đồng thời, có thể bổ sung hoặc thậm chí là ưu tiên tùy thuộc vào bối cảnh gỡ lỗi.
