---
title:                "Sử dụng bộ gỡ lỗi"
date:                  2024-01-28T22:09:09.235288-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng bộ gỡ lỗi"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c/using-a-debugger.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại sao?
Trình gỡ lỗi là công cụ cho phép bạn kiểm tra mã C của mình khi nó chạy, từng bước một, để tìm và khắc phục lỗi. Các lập trình viên sử dụng trình gỡ lỗi để hiểu cách mã của họ hoạt động, sửa chữa vấn đề và tối ưu hóa hiệu suất mà không cần phải đoán mò.

## Cách thức:
Giả sử bạn đang làm việc với một chương trình C đơn giản tính giai thừa của một số, nhưng có một sự cố. Để sử dụng trình gỡ lỗi như `gdb` (GNU Debugger), trước tiên hãy biên dịch với cờ `-g` để bao gồm thông tin gỡ lỗi:

```c
// biên dịch với: gcc factorial.c -o factorial -g
#include <stdio.h>

long factorial(int n) {
    if (n < 0) return 0; // Một kiểm tra đơn giản cho đầu vào âm
    long result = 1;
    while (n > 1)
        result *= n--;
    return result;
}

int main() {
    int number = 5;
    long result = factorial(number);
    printf("Giai thừa của %d là %ld\n", number, result);
    return 0;
}
```

Sau đó, chạy nó trong gdb:

```shell
$ gdb ./factorial
```

Đặt một điểm dừng tại hàm `factorial` và chạy chương trình:

```gdb
(gdb) break factorial
(gdb) run
```

Khi đến điểm dừng, bước qua từng dòng một sử dụng `next` hoặc `n` và kiểm tra biến với `print` hoặc `p`:

```gdb
(gdb) next
(gdb) print result
$1 = 1
```

Kết quả mẫu sẽ cung cấp giá trị thực tế và dòng chảy thực thi của chương trình.

## Sâu hơn
Trình gỡ lỗi đã tồn tại từ những năm 1960, phát triển từ những màn hình giám sát đơn giản đến các ứng dụng phức tạp dựa trên giao diện đồ họa. Kỹ thuật gỡ lỗi dựa vào in ấn thông tin ra màn hình đã phổ biến trước khi các trình gỡ lỗi hoàn chỉnh được phát triển. Các lựa chọn khác cho `gdb` bao gồm `lldb`, `dbx`, hoặc trình gỡ lỗi tích hợp trong IDE như trong Visual Studio hoặc CLion.

Khi làm việc với trình gỡ lỗi, cách thực hiện có thể khác nhau—một số có thể bắt lỗi thời gian chạy, kiểm tra bộ nhớ, hoặc thậm chí đảo ngược quá trình thực thi của chương trình. `gdb` có thể được gắn vào các quy trình đang chạy, cho phép gỡ lỗi phần mềm đang chạy, một lợi ích để sửa chữa lỗi hệ thống trực tiếp.

## Xem thêm
- GNU Debugger (GDB): https://www.gnu.org/software/gdb/documentation/
- Gỡ lỗi với GDB: https://sourceware.org/gdb/current/onlinedocs/gdb
- LLDB Debugger: https://lldb.llvm.org/use/tutorial.html
- Kỹ thuật gỡ lỗi trong C: http://www.cprogramming.com/debugging/debugging.html
