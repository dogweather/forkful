---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:21.876660-07:00
description: "L\xE0m th\u1EBF n\xE0o: D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9t kh\u1ED1\
  i c\u01A1 b\u1EA3n try-catch \u0111\u1EC3 x\u1EED l\xFD m\u1ED9t ngo\u1EA1i l\u1EC7\
  ."
lastmod: '2024-03-13T22:44:37.052361-06:00'
model: gpt-4-0125-preview
summary: "D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9t kh\u1ED1i c\u01A1 b\u1EA3n try-catch\
  \ \u0111\u1EC3 x\u1EED l\xFD m\u1ED9t ngo\u1EA1i l\u1EC7."
title: "X\u1EED l\xFD l\u1ED7i"
weight: 16
---

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
