---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:56.307963-07:00
description: "Chuy\u1EC3n \u0111\u1ED5i m\u1ED9t chu\u1ED7i sang ch\u1EEF th\u01B0\
  \u1EDDng l\xE0m thay \u0111\u1ED5i t\u1EA5t c\u1EA3 c\xE1c ch\u1EEF c\xE1i trong\
  \ chu\u1ED7i \u0111\xF3 th\xE0nh d\u1EA1ng ch\u1EEF th\u01B0\u1EDDng. L\u1EADp tr\xEC\
  nh vi\xEAn th\u1EF1c hi\u1EC7n vi\u1EC7c n\xE0y \u0111\u1EC3 \u0111\u1EA3m b\u1EA3\
  o\u2026"
lastmod: 2024-02-19 22:04:56.423312
model: gpt-4-0125-preview
summary: "Chuy\u1EC3n \u0111\u1ED5i m\u1ED9t chu\u1ED7i sang ch\u1EEF th\u01B0\u1EDD\
  ng l\xE0m thay \u0111\u1ED5i t\u1EA5t c\u1EA3 c\xE1c ch\u1EEF c\xE1i trong chu\u1ED7\
  i \u0111\xF3 th\xE0nh d\u1EA1ng ch\u1EEF th\u01B0\u1EDDng. L\u1EADp tr\xECnh vi\xEA\
  n th\u1EF1c hi\u1EC7n vi\u1EC7c n\xE0y \u0111\u1EC3 \u0111\u1EA3m b\u1EA3o\u2026"
title: "Chuy\u1EC3n \u0111\u1ED5i chu\u1ED7i th\xE0nh ch\u1EEF th\u01B0\u1EDDng"
---

{{< edit_this_page >}}

## Cái Gì và Tại Sao?

Chuyển đổi một chuỗi sang chữ thường làm thay đổi tất cả các chữ cái trong chuỗi đó thành dạng chữ thường. Lập trình viên thực hiện việc này để đảm bảo tính nhất quán, so sánh, sắp xếp, hoặc đáp ứng yêu cầu về độ nhạy cảm với chữ hoa chữ thường của một số hệ thống.

## Cách thực hiện:

Sử dụng lệnh `string`, việc chuyển đổi văn bản sang chữ thường trở nên đơn giản. Chỉ cần thực hiện:

```Fish Shell
echo "MAKE ME LOWERCASE" | string lower
```

Kết quả mẫu:

```
make me lowercase
```

Đối với một biến:

```Fish Shell
set my_string "SHOUTY CASE TEXT"
string lower -q -- $my_string
```

Kết quả:

```
shouty case text
```

## Đào Sâu Hơn:

Trước khi có Fish Shell, người dùng Unix thường sử dụng `tr '[:upper:]' '[:lower:]'` hoặc `awk '{print tolower($0)}'`. Mặc dù chúng hoạt động, nhưng không đơn giản và trực tiếp như chức năng `string lower` được tích hợp sẵn trong Fish.

Fish giới thiệu `string` trong phiên bản v2.3.0 (Tháng 5 năm 2016), nâng cao việc xử lý chuỗi lên là thành phần cốt lõi của shell, thay vì yêu cầu các lệnh bên ngoài. Điều này làm tăng tính đơn giản và tốc độ cho các tác vụ phổ biến như chuyển đổi chữ cái.

Tại sao không chỉ sử dụng `tr` hoặc `awk`? `string lower` được tích hợp sẵn trong Fish, có nghĩa là nó nhanh hơn (không cần khởi chạy các quy trình mới) và hoạt động một cách nhất quán và dự đoán trước được trên các hệ thống khác nhau. Nó cũng là một phần của bộ lệnh `string` rộng lớn hơn xử lý các thao tác chuỗi khác, giúp việc viết kịch bản gọn gàng và hiệu quả hơn.

## Xem Thêm:

- Tài liệu chính thức của `string`: https://fishshell.com/docs/current/cmds/string.html
- Kho lưu trữ GitHub của Fish Shell: https://github.com/fish-shell/fish-shell
- Bối cảnh lịch sử và so sánh của `string` so với các lệnh Unix truyền thống: https://github.com/fish-shell/fish-shell/issues/159
