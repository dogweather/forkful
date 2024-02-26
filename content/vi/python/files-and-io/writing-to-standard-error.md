---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:14:06.450478-07:00
description: "L\u1ED7i chu\u1EA9n, th\u01B0\u1EDDng \u0111\u01B0\u1EE3c g\u1ECDi l\xE0\
  \ stderr, l\xE0 m\u1ED9t lu\u1ED3ng t\u1EADp tin \u0111\u01B0\u1EE3c \u0111\u1ECB\
  nh ngh\u0129a tr\u01B0\u1EDBc \u0111\u1EC3 ghi nh\u1EADt k\xFD c\xE1c th\xF4ng b\xE1\
  o l\u1ED7i. L\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng n\xF3 \u0111\u1EC3 ph\xE2\
  n bi\u1EC7t\u2026"
lastmod: '2024-02-25T18:49:34.494718-07:00'
model: gpt-4-0125-preview
summary: "L\u1ED7i chu\u1EA9n, th\u01B0\u1EDDng \u0111\u01B0\u1EE3c g\u1ECDi l\xE0\
  \ stderr, l\xE0 m\u1ED9t lu\u1ED3ng t\u1EADp tin \u0111\u01B0\u1EE3c \u0111\u1ECB\
  nh ngh\u0129a tr\u01B0\u1EDBc \u0111\u1EC3 ghi nh\u1EADt k\xFD c\xE1c th\xF4ng b\xE1\
  o l\u1ED7i. L\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng n\xF3 \u0111\u1EC3 ph\xE2\
  n bi\u1EC7t\u2026"
title: "Ghi v\xE0o l\u1ED7i chu\u1EA9n"
---

{{< edit_this_page >}}

## Gì và Tại sao?
Lỗi chuẩn, thường được gọi là stderr, là một luồng tập tin được định nghĩa trước để ghi nhật ký các thông báo lỗi. Lập trình viên sử dụng nó để phân biệt đầu ra chương trình thường xuyên với các thông báo lỗi, giúp việc gỡ lỗi trở nên dễ dàng hơn.

## Làm thế nào:
Để viết vào stderr trong Python:

```Python
import sys

print("Đây là một thông điệp bình thường.")
sys.stderr.write("Đây là một thông báo lỗi.\n")
```

Kết quả Mẫu:
```
Đây là một thông điệp bình thường.
Đây là một thông báo lỗi.
```

Lưu ý rằng trong khi `print()` thêm một dòng mới mặc định, `sys.stderr.write()` thì không—bạn cần phải bao gồm `\n` để bắt đầu một dòng mới.

## Đi sâu hơn
Theo lịch sử, các luồng chuẩn được giới thiệu trong Unix. Có ba luồng: đầu vào chuẩn (`stdin`), đầu ra chuẩn (`stdout`), và lỗi chuẩn (`stderr`). Trong Python, module `sys` cung cấp quyền truy cập vào các luồng này. Trong khi `stdout` thường được sử dụng cho đầu ra chính của chương trình, `stderr` được dành riêng cho các thông báo lỗi và chẩn đoán.

Các lựa chọn khác cho `sys.stderr.write()` bao gồm sử dụng `print()` với đối số `file`:

```Python
print("Đây là một thông báo lỗi.", file=sys.stderr)
```

Điều này thực hiện tương tự nhưng tận dụng các tính năng thân thiện với người dùng của `print()`. Về cơ chế nội bộ, cả hai phương pháp cuối cùng đều thực hiện các lời gọi viết ở cấp độ hệ thống vào luồng tương ứng.

## Xem thêm
- Tài liệu Python cho module sys: https://docs.python.org/3/library/sys.html
- Các Luồng Chuẩn Unix: https://en.wikipedia.org/wiki/Standard_streams
- Thảo luận về việc sử dụng stderr: https://stackoverflow.com/questions/5574702/how-to-print-to-stderr-in-python
