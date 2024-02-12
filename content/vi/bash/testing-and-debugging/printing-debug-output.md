---
title:                "In ấn thông tin gỡ lỗi"
date:                  2024-01-28T22:04:23.643823-07:00
model:                 gpt-4-0125-preview
simple_title:         "In ấn thông tin gỡ lỗi"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/bash/printing-debug-output.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Điều gì & Tại sao?

In đầu ra để gỡ lỗi chủ yếu liên quan đến việc hiển thị dữ liệu lên console để kiểm tra xem điều gì đang diễn ra trong script của bạn. Lập trình viên thực hiện điều này để theo dõi các biến, theo dõi luồng logic, và tìm ra những lỗi khó chịu.

## Cách thực hiện:

```Bash
#!/bin/bash

# Định nghĩa một biến
name="Gizmo"

# In biến để gỡ lỗi
echo "Debug: Tên biến là $name"

# Điều kiện với đầu ra gỡ lỗi
if [[ $name == "Gizmo" ]]; then
    echo "Debug: Đã vào câu lệnh if."
    # Thực hiện một hành động nào đó
fi

# Vòng lặp với đầu ra gỡ lỗi
for i in {1..3}; do
    echo "Debug: Lần lặp vòng lặp $i"
    # Thực hiện một hành động trong vòng lặp
done
```

Đầu ra:
```
Debug: Tên biến là Gizmo
Debug: Đã vào câu lệnh if.
Debug: Lần lặp vòng lặp 1
Debug: Lần lặp vòng lặp 2
Debug: Lần lặp vòng lặp 3
```

## Sâu hơn

Ban đầu, việc gỡ lỗi có nghĩa là loại bỏ các lỗi cơ học thật sự làm gián đoạn các máy tính đầu tiên. Ngày nay, nó liên quan đến việc khắc phục lỗi trong code. Đầu ra gỡ lỗi là kính lúp của lập trình viên.

Các lựa chọn thay thế cho `echo` trong các script bash bao gồm `printf` cho nhiều tùy chọn định dạng hơn, hoặc viết vào một tệp với việc chuyển hướng `>` để có nhật ký bền vững.

Bash cũng hỗ trợ đầu ra gỡ lỗi có điều kiện với lệnh nội bộ `set -x` để theo dõi các lệnh và các đối số của chúng khi được thực thi. `set -x` rất hữu ích cho việc gỡ lỗi toàn bộ script.

## Xem thêm

- Trang `man` của Bash: `man bash`
- Hướng dẫn script nâng cao: [Bash Guide for Beginners by Machtelt Garrels](https://tldp.org/LDP/Bash-Beginners-Guide/html/)
- Stack Overflow để khắc phục sự cố: [stackoverflow.com](https://stackoverflow.com/questions/tagged/bash)
