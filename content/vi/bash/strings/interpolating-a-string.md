---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:16.672741-07:00
description: "N\u1ED9i suy chu\u1ED7i cho ph\xE9p b\u1EA1n ch\xE8n gi\xE1 tr\u1ECB\
  \ v\xE0o m\u1ED9t chu\u1ED7i. N\xF3 r\u1EA5t ti\u1EC7n l\u1EE3i cho vi\u1EC7c t\u1EA1\
  o ra c\xE1c tin nh\u1EAFn t\xF9y ch\u1EC9nh, t\u1EF1 \u0111\u1ED9ng h\xF3a l\u1EC7\
  nh v\xE0 vi\u1EBFt k\u1ECBch b\u1EA3n nh\u01B0 m\u1ED9t\u2026"
lastmod: '2024-02-25T18:49:35.202805-07:00'
model: gpt-4-0125-preview
summary: "N\u1ED9i suy chu\u1ED7i cho ph\xE9p b\u1EA1n ch\xE8n gi\xE1 tr\u1ECB v\xE0\
  o m\u1ED9t chu\u1ED7i. N\xF3 r\u1EA5t ti\u1EC7n l\u1EE3i cho vi\u1EC7c t\u1EA1o\
  \ ra c\xE1c tin nh\u1EAFn t\xF9y ch\u1EC9nh, t\u1EF1 \u0111\u1ED9ng h\xF3a l\u1EC7\
  nh v\xE0 vi\u1EBFt k\u1ECBch b\u1EA3n nh\u01B0 m\u1ED9t\u2026"
title: "N\u1ED9i suy chu\u1ED7i k\xFD t\u1EF1"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Nội suy chuỗi cho phép bạn chèn giá trị vào một chuỗi. Nó rất tiện lợi cho việc tạo ra các tin nhắn tùy chỉnh, tự động hóa lệnh và viết kịch bản như một ông chủ. 

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
