---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:16.672741-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Chu\u1ED7i trong Bash r\u1EA5t d\u1EC5\
  \ d\xE0ng s\u1EED d\u1EE5ng v\u1EDBi bi\u1EBFn. Ch\u1EC9 c\u1EA7n \u0111\u01B0a\
  \ m\u1ED9t bi\u1EBFn v\xE0o trong chu\u1ED7i v\u1EDBi m\u1ED9t v\xE0i d\u1EA5u ngo\u1EB7\
  c nh\u1ECDn, b\u1EA1n \u0111\xE3 ho\xE0n th\xE0nh xong."
lastmod: '2024-03-13T22:44:36.858542-06:00'
model: gpt-4-0125-preview
summary: "Chu\u1ED7i trong Bash r\u1EA5t d\u1EC5 d\xE0ng s\u1EED d\u1EE5ng v\u1EDB\
  i bi\u1EBFn."
title: "N\u1ED9i suy chu\u1ED7i k\xFD t\u1EF1"
weight: 8
---

## Cách thực hiện:
Chuỗi trong Bash rất dễ dàng sử dụng với biến. Chỉ cần đưa một biến vào trong chuỗi với một vài dấu ngoặc nhọn, bạn đã hoàn thành xong.

```Bash
name="World"
greeting="Hello, ${name}!"
echo $greeting
```

Đầu ra:
```
Hello, World!
```

Bash nói, "Hãy giữ nó linh hoạt." Thay đổi `name`, và lời chào của bạn cũng thay đổi theo.

```Bash
name="Bash Pros"
greeting="Hello, ${name}!"
echo $greeting
```

Đầu ra:
```
Hello, Bash Pros!
```

## Đào sâu:
Ngày xưa, các lập trình viên ghép chuỗi lại với nhau bằng cách nối chuỗi. Điều này trở nên lộn xộn. Nội suy chuỗi đã xuất hiện như một siêu anh hùng cho mã nguồn sạch sẽ, dễ đọc hơn.

Bash, không giống như một số ngôn ngữ khác, không làm phiền—chỉ cần một dấu đô la và một vài dấu ngoặc nhọn. Các ngôn ngữ khác trang điểm nó với cú pháp đặc biệt hoặc các hàm. Trong Bash, tất cả đều xoay quanh những dấu ngoặc nhọn và đôi khi là ký tự thoát nếu bạn cảm thấy sang trọng.

Một số lựa chọn khác? Chắc chắn rồi, bạn có thể nối chuỗi hoặc sử dụng `echo` không cần dấu ngoặc nhọn nếu bạn không thực hiện gì phức tạp. Nhưng tại sao phải chấp nhận sự đơn giản đó?

Về cách triển khai, khi Bash thấy `${}`, nó lấy giá trị của biến và thay thế nó vào, không hỏi bất cứ điều gì. Điều này đảm bảo những gì bạn thấy (trong mã của bạn) là những gì bạn nhận được (trong đầu ra).

## Xem thêm
Để biết thêm về phép thuật chuỗi:

- Sự Thao túng Chuỗi Bash: https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html
- Hướng dẫn Lập trình Bash nâng cao: https://tldp.org/LDP/abs/html/
- Stack Overflow (ví dụ thực tế cho các vấn đề trong thế giới thực): https://stackoverflow.com/questions/tagged/bash
