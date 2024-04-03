---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:25.896433-07:00
description: "L\xE0m th\u1EBF n\xE0o: GDB (GNU Debugger) l\xE0 c\xF4ng c\u1EE5 debugger\
  \ \u0111\u01B0\u1EE3c s\u1EED d\u1EE5ng ph\u1ED5 bi\u1EBFn nh\u1EA5t cho l\u1EAD\
  p tr\xECnh C. D\u01B0\u1EDBi \u0111\xE2y l\xE0 h\u01B0\u1EDBng d\u1EABn ng\u1EAF\
  n g\u1ECDn v\u1EC1 vi\u1EC7c s\u1EED d\u1EE5ng GDB \u0111\u1EC3 g\u1EE1\u2026"
lastmod: '2024-03-13T22:44:37.274313-06:00'
model: gpt-4-0125-preview
summary: "GDB (GNU Debugger) l\xE0 c\xF4ng c\u1EE5 debugger \u0111\u01B0\u1EE3c s\u1EED\
  \ d\u1EE5ng ph\u1ED5 bi\u1EBFn nh\u1EA5t cho l\u1EADp tr\xECnh C."
title: "S\u1EED d\u1EE5ng tr\xECnh g\u1EE1 l\u1ED7i"
weight: 35
---

## Làm thế nào:
GDB (GNU Debugger) là công cụ debugger được sử dụng phổ biến nhất cho lập trình C. Dưới đây là hướng dẫn ngắn gọn về việc sử dụng GDB để gỡ lỗi một chương trình C đơn giản.

Trước tiên, biên dịch chương trình C của bạn với cờ `-g` để bao gồm thông tin gỡ lỗi:

```c
gcc -g program.c -o program
```

Tiếp theo, bắt đầu GDB với chương trình đã biên dịch của bạn:

```bash
gdb ./program
```

Bây giờ, bạn có thể sử dụng các lệnh khác nhau trong GDB để điều khiển hoạt động của nó. Dưới đây là một số lệnh cơ bản:

- `break`: Đặt một điểm dừng tại một dòng hoặc hàm đã chỉ định để tạm dừng thực thi.
  - Ví dụ: `break 10` hoặc `break main`
- `run`: Bắt đầu thực thi chương trình của bạn trong GDB.
- `next`: Thực thi dòng tiếp theo của mã mà không bước vào các hàm.
- `step`: Thực thi dòng tiếp theo của mã, bước vào các hàm.
- `print`: Hiển thị giá trị của một biến.
- `continue`: Tiếp tục thực thi cho đến điểm dừng tiếp theo.
- `quit`: Thoát khỏi GDB.

Dưới đây là một phiên làm việc ví dụ về việc gỡ lỗi một chương trình đơn giản:

```c
#include <stdio.h>

int main() {
    int i;
    for (i = 0; i < 5; i++) {
        printf("%d\n", i);
    }
    return 0;
}
```

Biên dịch và bắt đầu GDB như đã mô tả. Đặt một điểm dừng tại dòng `printf` với `break 5` sau đó `run`. Sử dụng `next` để bước qua vòng lặp và `print i` để kiểm tra biến vòng lặp.

Kết quả mẫu sau khi đặt điểm dừng và trước lần lặp đầu tiên:

```
Breakpoint 1, main () at program.c:5
5         printf("%d\n", i);
```

Sử dụng `print i` sau một vài lần lặp:

```
$3 = 2
```

Điều này minh họa việc xem xét trạng thái và luồng của một chương trình đơn giản.

## Tìm hiểu sâu hơn
Khái niệm về gỡ lỗi đã phát triển đáng kể kể từ những ngày đầu của lập trình, nơi mà các lỗi vật lý (côn trùng thực sự) có thể gây ra vấn đề trong máy tính cơ học. Ngày nay, những trình gỡ lỗi như GDB cung cấp những tính năng tiên tiến hơn ngoài việc bước qua và kiểm tra biến, chẳng hạn như gỡ lỗi ngược (thực thi chương trình theo hướng ngược lại), điểm dừng có điều kiện, và viết kịch bản cho các nhiệm vụ gỡ lỗi tự động.

Mặc dù GDB mạnh mẽ và được sử dụng rộng rãi, nó có thể phức tạp và khó khăn cho người mới bắt đầu. Các công cụ và môi trường phát triển tích hợp (IDEs) thay thế như Visual Studio Code, CLion, hoặc Eclipse cung cấp các giao diện thân thiện hơn cho việc gỡ lỗi mã C, thường kết hợp các phụ trợ trực quan và điều khiển trực quan hơn. Những sự thay thế này có thể không cung cấp đầy đủ chiều sâu chức năng của GDB nhưng có thể dễ tiếp cận hơn với người mới bắt đầu lập trình C.

Hơn nữa, sự xuất hiện của các giao thức máy chủ ngôn ngữ và tiêu chuẩn gỡ lỗi đã tạo điều kiện cho các giải pháp gỡ lỗi đa nền tảng, làm cho trải nghiệm gỡ lỗi trở nên nhất quán hơn qua các công cụ và môi trường khác nhau. Mặc dù có những tiến bộ này, việc học hỏi những điều cơ bản và sâu sắc của một trình gỡ lỗi truyền thống như GDB cung cấp cái nhìn sâu sắc về việc thực thi của chương trình C và vẫn là kỹ năng quan trọng trong bộ công cụ của nhà phát triển.
