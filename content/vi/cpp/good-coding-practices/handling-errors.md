---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:21.876660-07:00
description: "X\u1EED l\xFD l\u1ED7i c\xF3 ngh\u0129a l\xE0 l\xEAn k\u1EBF ho\u1EA1\
  ch cho nh\u1EEFng khi m\u1ECDi th\u1EE9 di\u1EC5n ra kh\xF4ng nh\u01B0 mong \u0111\
  \u1EE3i. \u0110i\u1EC1u n\xE0y quan tr\u1ECDng v\xEC n\xF3 gi\xFAp tr\xE1nh g\xE2\
  y s\u1EADp ch\u01B0\u01A1ng tr\xECnh v\xE0 l\xE0m cho\u2026"
lastmod: '2024-02-25T18:49:35.399175-07:00'
model: gpt-4-0125-preview
summary: "X\u1EED l\xFD l\u1ED7i c\xF3 ngh\u0129a l\xE0 l\xEAn k\u1EBF ho\u1EA1ch\
  \ cho nh\u1EEFng khi m\u1ECDi th\u1EE9 di\u1EC5n ra kh\xF4ng nh\u01B0 mong \u0111\
  \u1EE3i. \u0110i\u1EC1u n\xE0y quan tr\u1ECDng v\xEC n\xF3 gi\xFAp tr\xE1nh g\xE2\
  y s\u1EADp ch\u01B0\u01A1ng tr\xECnh v\xE0 l\xE0m cho\u2026"
title: "X\u1EED l\xFD l\u1ED7i"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Xử lý lỗi có nghĩa là lên kế hoạch cho những khi mọi thứ diễn ra không như mong đợi. Điều này quan trọng vì nó giúp tránh gây sập chương trình và làm cho phần mềm của bạn robust và thân thiện với người dùng.

## Làm thế nào:
Dưới đây là một khối cơ bản try-catch để xử lý một ngoại lệ:

```cpp
#include <iostream>
#include <stdexcept>

int main() {
    try {
        throw std::runtime_error("Ối! Có gì đó không ổn.");
    } catch (const std::exception& e) {
        std::cerr << "Lỗi: " << e.what() << std::endl;
    }
    return 0;
}
```

Kết quả mẫu:
```
Lỗi: Ối! Có gì đó không ổn.
```

## Sâu hơn
C++ đã có xử lý lỗi ngay từ những ngày đầu. Hình thức cơ bản nhất là kiểm tra giá trị trả về. Nếu bạn đã trải qua những ngày đó, bạn sẽ nhớ về thời kỳ trước chuẩn: C với các lớp và kiểm tra lỗi bằng tay.

Sau đó, ngoại lệ được giới thiệu vào C++ để cung cấp một cách có cấu trúc để đối phó với những vấn đề không mong đợi. Một ngoại lệ được ném ra với `throw` và bắt với `try/catch`.

Có hai loại lỗi thường xuất hiện: lỗi logic, như một phép tính sai, và lỗi thời gian chạy, như việc truy cập vào địa chỉ bộ nhớ không hợp lệ. Ngoại lệ lý tưởng cho lỗi thời gian chạy. Đối với lỗi logic, thường sử dụng đảm bảo hoặc mã lỗi là tốt hơn.

Có một cuộc tranh luận liên tục về ngoại lệ so với mã lỗi. Ngoại lệ có thể chậm hơn và có thể dẫn đến dòng điều khiển phức tạp. Mã lỗi, trong khi nhanh hơn, có thể làm cho mã trở nên rối rắm và khó bảo trì hơn. Đó là một sự đánh đổi, vì vậy biết trường hợp sử dụng của bạn là chìa khóa.

C++17 đã giới thiệu `std::optional` và `std::variant`, là những phương án thay thế cho ngoại lệ. Chúng hữu ích cho các hàm có thể hoặc không thể trả về kết quả hợp lệ.

An toàn ngoại lệ có thể là một cơn đau đầu khác. Đó là về những bảo đảm mà mã của bạn cung cấp dù có ngoại lệ. Có ba mức độ: cơ bản, mạnh mẽ, và không ném lỗi. Càng nhiều bảo đảm, mã của bạn có thể càng phức tạp.

Nhận xét cuối cùng—xử lý lỗi cũng nhiều nghệ thuật như khoa học. Điều đó hình thành cách ứng dụng của bạn tồn tại trong thực tế. Đừng lạm dụng ngoại lệ. Hướng tới việc viết mã dễ đọc, dễ bảo trì.

## Xem thêm
- [cppreference về xử lý ngoại lệ](https://en.cppreference.com/w/cpp/language/exceptions)
- [Quan điểm của Bjarne Stroustrup về xử lý lỗi](http://www.stroustrup.com/except.pdf)
- [C++ Core Guidelines về ngoại lệ](https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines#Re-exceptions)
