---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:28.799151-07:00
description: "Vi\u1EC7c t\xECm chi\u1EC1u d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i ngh\u0129\
  a l\xE0 \u0111\u1EBFm s\u1ED1 l\u01B0\u1EE3ng k\xFD t\u1EF1 c\u1EE7a n\xF3. C\xE1\
  c l\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 x\xE1c th\u1EF1\
  c \u0111\u1EA7u v\xE0o, l\u1EB7p qua c\xE1c k\xFD t\u1EF1, ho\u1EB7c \u0111\u01A1\
  n gi\u1EA3n\u2026"
lastmod: '2024-02-25T18:49:35.208753-07:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c t\xECm chi\u1EC1u d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i ngh\u0129\
  a l\xE0 \u0111\u1EBFm s\u1ED1 l\u01B0\u1EE3ng k\xFD t\u1EF1 c\u1EE7a n\xF3. C\xE1\
  c l\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 x\xE1c th\u1EF1\
  c \u0111\u1EA7u v\xE0o, l\u1EB7p qua c\xE1c k\xFD t\u1EF1, ho\u1EB7c \u0111\u01A1\
  n gi\u1EA3n\u2026"
title: "T\xECm chi\u1EC1u d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i k\xFD t\u1EF1"
---

{{< edit_this_page >}}

## Gì & Tại Sao?
Việc tìm chiều dài của một chuỗi nghĩa là đếm số lượng ký tự của nó. Các lập trình viên làm điều này để xác thực đầu vào, lặp qua các ký tự, hoặc đơn giản là để căn chỉnh đầu ra.

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
