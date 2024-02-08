---
title:                "In ra thông tin gỡ lỗi"
aliases:
- vi/cpp/printing-debug-output.md
date:                  2024-01-28T22:04:43.758598-07:00
model:                 gpt-4-0125-preview
simple_title:         "In ra thông tin gỡ lỗi"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/cpp/printing-debug-output.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Là gì và Tại sao?
In thông điệp gỡ lỗi giống như việc trò chuyện với mã lệnh của bạn; bạn rắc vào các câu lệnh in để kiểm tra sức khỏe và tư duy của nó. Lập trình viên làm điều này để tìm và khắc phục lỗi hoặc để đảm bảo mọi thứ đang diễn ra suôn sẻ—giống như đang kiểm tra nhanh mã lệnh của bạn.

## Làm thế nào:
Dưới đây là một đoạn mã cho thấy cách in một thông điệp gỡ lỗi đơn giản ra bảng điều khiển.

```C++
#include <iostream>

int main() {
    int lifeTheUniverseAndEverything = 42;

    // Thông điệp gỡ lỗi
    std::cout << "Debug: Giá trị của lifeTheUniverseAndEverything là " 
              << lifeTheUniverseAndEverything << std::endl;

    // Phần còn lại của mã lệnh ở đây...

    return 0;
}
```

Kết quả mẫu:
```
Debug: Giá trị của lifeTheUniverseAndEverything là 42
```

## Đào sâu
Từ lâu, thông điệp gỡ lỗi đã được khắc lên các phương tiện vật lý. Không thú vị lắm. Bây giờ, chúng ta chỉ cần sử dụng `std::cout` và các công cụ tương tự. `std::cerr` dùng cho các lỗi, thường được sử dụng cùng với `std::cout`. Tại sao lại có hai luồng khác nhau? Đó giống như việc có các cuộc trò chuyện khác nhau cho công việc và bạn bè; nó giúp mọi thứ được tổ chức gọn gàng. Các IDE hiện đại cung cấp bộ gỡ lỗi tích hợp, nhưng đôi khi một câu lệnh in đơn giản cũng có hiệu quả mà không cần phải rườm rà. Hãy cảnh giác, in không cần thiết làm chậm mọi thứ; hãy tưởng tượng ai đó đang miêu tả từng bước họ thực hiện. Dọn dẹp khi bạn hoàn thành.

## Xem Thêm
- [cppreference.com](https://en.cppreference.com/w/cpp/io/cout) – để học sâu hơn về `std::cout` và bạn bè.
- [GNU Project Debugger (GDB)](https://www.gnu.org/software/gdb/) - khi bạn đã sẵn sàng vượt qua việc in để tiến tới một trình gỡ lỗi đầy đủ.
- [Stack Overflow](https://stackoverflow.com/questions/tagged/c%2b%2b) – để xem những vấn đề mà người khác đã gặp phải và cách gỡ lỗi bằng cách in có thể giúp ích.
