---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:43.758598-07:00
description: "L\xE0m th\u1EBF n\xE0o: D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9t \u0111\
  o\u1EA1n m\xE3 cho th\u1EA5y c\xE1ch in m\u1ED9t th\xF4ng \u0111i\u1EC7p g\u1EE1\
  \ l\u1ED7i \u0111\u01A1n gi\u1EA3n ra b\u1EA3ng \u0111i\u1EC1u khi\u1EC3n."
lastmod: '2024-03-13T22:44:37.045984-06:00'
model: gpt-4-0125-preview
summary: "D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9t \u0111o\u1EA1n m\xE3 cho th\u1EA5\
  y c\xE1ch in m\u1ED9t th\xF4ng \u0111i\u1EC7p g\u1EE1 l\u1ED7i \u0111\u01A1n gi\u1EA3\
  n ra b\u1EA3ng \u0111i\u1EC1u khi\u1EC3n."
title: "In ra th\xF4ng tin g\u1EE1 l\u1ED7i"
weight: 33
---

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
