---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:36.646535-07:00
description: "M\u1ED9t shell t\u01B0\u01A1ng t\xE1c, hay V\xF2ng l\u1EB7p \u0110\u1ECD\
  c-\u0110\xE1nh gi\xE1-In (REPL), cho ph\xE9p b\u1EA1n th\u1EED nghi\u1EC7m v\u1EDB\
  i m\xE3 Go trong th\u1EDDi gian th\u1EF1c, th\u1EF1c thi c\xE1c l\u1EC7nh v\xE0\
  \ nh\u1EADn ph\u1EA3n h\u1ED3i ngay\u2026"
lastmod: '2024-03-13T22:44:35.984345-06:00'
model: gpt-4-0125-preview
summary: "M\u1ED9t shell t\u01B0\u01A1ng t\xE1c, hay V\xF2ng l\u1EB7p \u0110\u1ECD\
  c-\u0110\xE1nh gi\xE1-In (REPL), cho ph\xE9p b\u1EA1n th\u1EED nghi\u1EC7m v\u1EDB\
  i m\xE3 Go trong th\u1EDDi gian th\u1EF1c, th\u1EF1c thi c\xE1c l\u1EC7nh v\xE0\
  \ nh\u1EADn ph\u1EA3n h\u1ED3i ngay l\u1EADp t\u1EE9c."
title: "S\u1EED d\u1EE5ng giao di\u1EC7n d\xF2ng l\u1EC7nh t\u01B0\u01A1ng t\xE1c\
  \ (REPL)"
weight: 34
---

## Cái gì và Tại sao?

Một shell tương tác, hay Vòng lặp Đọc-Đánh giá-In (REPL), cho phép bạn thử nghiệm với mã Go trong thời gian thực, thực thi các lệnh và nhận phản hồi ngay lập tức. Phương pháp này được sử dụng rộng rãi để học hỏi, gỡ lỗi và tạo mẫu, vì nó bỏ qua chu kỳ chỉnh sửa-biên dịch-chạy truyền thống, làm cho quy trình phát triển nhanh chóng và trực quan hơn.

## Làm thế nào:

Mặc dù Go không bao gồm REPL được tích hợp sẵn, cộng đồng đã tạo ra các công cụ như `gore` để lấp đầy khoảng trống này. Đầu tiên, cài đặt `gore` bằng cách chạy:

```
$ go get -u github.com/motemen/gore
```

Một khi đã cài đặt, khởi chạy `gore` bằng cách gõ `gore` trong terminal của bạn:

```
$ gore
```

Bạn sẽ thấy một dấu nhắc sẵn sàng chấp nhận các lệnh Go. Hãy thử một ví dụ đơn giản:

```
gore> :import fmt
gore> fmt.Println("Hello, Go REPL!")
```

Bạn sẽ thấy đầu ra như:

```
Hello, Go REPL!
```

Các biến và định nghĩa hàm hoạt động như mong đợi. Bạn có thể khai báo một hàm:

```
gore> :import math
gore> areaCircle := func(radius float64) float64 {
...> return math.Pi * radius * radius
...> }
gore> fmt.Println("Diện tích của hình tròn có bán kính 4:", areaCircle(4))
```

Và nhận đầu ra ngay lập tức:

```
Diện tích của hình tròn có bán kính 4: 50.26548245743669
```

## Sâu hơn nữa:

Khái niệm về REPL là cổ xưa, trở lại với các máy Lisp của những năm 1960, cung cấp một trải nghiệm lập trình tương tác. Không giống như các ngôn ngữ như Python hay JavaScript, Go được thiết kế không có REPL, tập trung thay vào đó vào các file nhị phân biên dịch vì hiệu suất và sự đơn giản. Điều này phản ánh triết lý về sự đơn giản và thiết kế của Go cho phần mềm có khả năng mở rộng và bảo trì.

Tuy nhiên, các công cụ như `gore` hay `goplay` cho thấy sự sáng tạo của cộng đồng Go trong việc lấp đầy khoảng trống này. Những công cụ này phân tích cú pháp mã Go một cách động và sử dụng gói `go/eval` hay các cơ chế tương tự để thực thi nó trong thời gian thực, mặc dù với một số giới hạn so với môi trường REPL bản địa. Những hạn chế này xuất phát từ hệ thống kiểu và mô hình biên dịch của Go, có thể khiến việc đánh giá tức thì trở nên thách thức.

Mặc dù môi trường REPL hết sức hữu ích cho giáo dục và các bài kiểm tra nhanh, hệ sinh thái Go thường nghiên cứu về quy trình biên dịch và chạy truyền thống cho hầu hết các nhiệm vụ phát triển. Các IDE và trình biên tập hỗ trợ Go, như Visual Studio Code hay GoLand, cung cấp các công cụ tích hợp cho việc kiểm tra và gỡ lỗi giảm bớt nhiều nhu cầu về REPL cho phát triển chuyên nghiệp.

Tuy nhiên, cho lập trình khám phá, tạo mẫu nhanh hoặc học hỏi, các REPL như `gore` cung cấp một lựa chọn giá trị, cho phép lập trình viên quen với REPL ở các ngôn ngữ khác có được trải nghiệm tương tự trong Go.
