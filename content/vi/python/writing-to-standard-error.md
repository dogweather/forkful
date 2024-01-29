---
title:                "Ghi vào lỗi chuẩn"
date:                  2024-01-28T22:14:06.450478-07:00
model:                 gpt-4-0125-preview
simple_title:         "Ghi vào lỗi chuẩn"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/python/writing-to-standard-error.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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
