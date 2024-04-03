---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:28.799151-07:00
description: "C\xE1ch Th\u1EF1c Hi\u1EC7n: Bi\u1EC3u t\u01B0\u1EE3ng `#` c\xF3 th\u1EC3\
  \ gi\xFAp b\u1EA1n ho\xE0n th\xE0nh c\xF4ng vi\u1EC7c trong bash. S\u1EED d\u1EE5\
  ng n\xF3 v\u1EDBi s\u1EF1 m\u1EDF r\u1ED9ng tham s\u1ED1. D\u01B0\u1EDBi \u0111\xE2\
  y l\xE0 c\xE1ch l\xE0m."
lastmod: '2024-03-13T22:44:36.864554-06:00'
model: gpt-4-0125-preview
summary: "Bi\u1EC3u t\u01B0\u1EE3ng `#` c\xF3 th\u1EC3 gi\xFAp b\u1EA1n ho\xE0n th\xE0\
  nh c\xF4ng vi\u1EC7c trong bash."
title: "T\xECm chi\u1EC1u d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i k\xFD t\u1EF1"
weight: 7
---

## Cách Thực Hiện:
Biểu tượng `#` có thể giúp bạn hoàn thành công việc trong bash. Sử dụng nó với sự mở rộng tham số. Dưới đây là cách làm:

```bash
my_string="Hello, World!"
string_length=${#my_string}
echo $string_length
```

Mẫu đầu ra:

```
13
```

## Sâu Hơn Nữa
Ngày xưa, mọi người sử dụng `expr` hoặc các công cụ bên ngoài như `wc -m`, nhưng các tính năng đã tích hợp sẵn trong Bash đã thay đổi cách chơi. Cú pháp `${#var}` là một phần của sự mở rộng tham số được giới thiệu trong Bash. Nó nhanh chóng và hiệu quả vì không phải tạo một tiểu trình con hay gọi một chương trình bên ngoài.

Có lựa chọn khác? Chắc chắn rồi:

- `expr length "$my_string"` cho bạn kết quả tương tự, nhưng nó hơi cũ kỹ.
- `echo -n $my_string | wc -m` sử dụng `wc` để đếm, nhưng nó quá mức cần thiết cho những tác vụ đơn giản.

Những chi tiết... Khi bạn sử dụng `${#my_string}`, nó mặc định cho bạn chiều dài tính bằng byte. Nếu văn bản của bạn đi trên con đường của unicode, bạn có thể cần phải xem xét đến các ký tự đa byte. Đó là khi mọi thứ trở nên phức tạp hơn.

## Xem Thêm
Lặn sâu vào các trang man với `man bash` để tìm hiểu kỹ hơn về sự mở rộng tham số. Đối với những ai muốn xử lý chuỗi ngoài ASCII cơ bản, Hướng Dẫn Scripting Bash Nâng Cao cung cấp một số cái nhìn: https://www.tldp.org/LDP/abs/html/. Và vì tình yêu với việc học hỏi, hãy theo dõi https://www.gnu.org/software/bash/manual/ để có thông tin mới nhất về Bash.
