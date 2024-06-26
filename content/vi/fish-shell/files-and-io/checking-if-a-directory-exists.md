---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:44.696696-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Ki\u1EC3m tra xem m\u1ED9t th\u01B0 m\u1EE5\
  c c\xF3 t\u1ED3n t\u1EA1i v\u1EDBi l\u1EC7nh `test` \u0111\u01A1n gi\u1EA3n."
lastmod: '2024-03-13T22:44:37.230072-06:00'
model: gpt-4-0125-preview
summary: "Ki\u1EC3m tra xem m\u1ED9t th\u01B0 m\u1EE5c c\xF3 t\u1ED3n t\u1EA1i v\u1EDB\
  i l\u1EC7nh `test` \u0111\u01A1n gi\u1EA3n."
title: "Ki\u1EC3m tra xem th\u01B0 m\u1EE5c c\xF3 t\u1ED3n t\u1EA1i kh\xF4ng"
weight: 20
---

## Cách thực hiện:
Kiểm tra xem một thư mục có tồn tại với lệnh `test` đơn giản:
```Fish Shell
if test -d /path/to/dir
    echo "Thư mục tồn tại"
else
    echo "Không có thư mục này"
end
```
Kết quả mẫu khi thư mục tồn tại:
```
Thư mục tồn tại
```
Kết quả mẫu khi thư mục không tồn tại:
```
Không có thư mục này
```

## Đi sâu vào vấn đề
Lệnh `test` (`[ ]` trong các shell POSIX) đã là một phần của hệ thống giống Unix trong nhiều thập kỷ. Trong Fish, `test -d` kiểm tra sự tồn tại của thư mục. Đây là một cách tiếp cận tốt hơn so với việc dựa vào kết quả từ các lệnh như `ls`, có thể không nhất quán hoặc quá chi tiết.

Các phương án khác:
- `status` có thể xác định xem một lệnh trước đó, như `cd /path/to/dir`, đã thành công hay không. Tuy nhiên, điều này không được khuyến nghị chỉ để kiểm tra sự tồn tại, vì nó thay đổi trạng thái của shell.
- Các công cụ bên ngoài như `find` hoặc ngôn ngữ lập trình (Python, Ruby) có thể thực hiện các nhiệm vụ tương tự nhưng thường là quá mức cho các kiểm tra đơn giản.

Thông tin chi tiết về việc triển khai:
Lệnh `test` có sẵn của Fish hiệu quả và đáng tin cậy. Nó tránh được những hạn chế thường gặp với việc gọi lệnh bên ngoài và cung cấp cú pháp dễ hiểu.

## Xem thêm
- Tài liệu Fish Shell về `test`: https://fishshell.com/docs/current/cmds/test.html
- Thông số kỹ thuật POSIX cho `test`: https://pubs.opengroup.org/onlinepubs/9699919799/utilities/test.html
- Thảo luận về việc kiểm tra sự tồn tại của tệp: https://unix.stackexchange.com/questions/590694/checking-if-a-directory-exists-in-unix-shell-scripting
