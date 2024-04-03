---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:56.307963-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: S\u1EED d\u1EE5ng l\u1EC7nh `string`, vi\u1EC7\
  c chuy\u1EC3n \u0111\u1ED5i v\u0103n b\u1EA3n sang ch\u1EEF th\u01B0\u1EDDng tr\u1EDF\
  \ n\xEAn \u0111\u01A1n gi\u1EA3n. Ch\u1EC9 c\u1EA7n th\u1EF1c hi\u1EC7n."
lastmod: '2024-03-13T22:44:37.190940-06:00'
model: gpt-4-0125-preview
summary: "S\u1EED d\u1EE5ng l\u1EC7nh `string`, vi\u1EC7c chuy\u1EC3n \u0111\u1ED5\
  i v\u0103n b\u1EA3n sang ch\u1EEF th\u01B0\u1EDDng tr\u1EDF n\xEAn \u0111\u01A1\
  n gi\u1EA3n."
title: "Chuy\u1EC3n \u0111\u1ED5i chu\u1ED7i th\xE0nh ch\u1EEF th\u01B0\u1EDDng"
weight: 4
---

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
