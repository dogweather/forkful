---
title:                "Viết các bài kiểm tra"
date:                  2024-01-28T22:12:54.422720-07:00
model:                 gpt-4-0125-preview
simple_title:         "Viết các bài kiểm tra"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/cpp/writing-tests.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Viết các bài kiểm tra giúp kiểm tra xem mã của bạn có thực hiện đúng như mong đợi hay không, bắt được lỗi sớm. Các lập trình viên thực hiện kiểm tra để tiết kiệm thời gian, giảm đau đầu và đảm bảo độ tin cậy.

## Làm thế nào:
Hãy sử dụng một hàm C++ đơn giản và một bài kiểm tra sử dụng framework Catch2.

```cpp
// main.cpp
#define CATCH_CONFIG_MAIN  // Cho phép Catch cung cấp main().
#include <catch2/catch.hpp>

int Add(int a, int b) {
    return a + b;
}

TEST_CASE( "Phép cộng hoạt động", "[toán]" ) {
    REQUIRE( Add(2, 2) == 4 );
}
```
Biên dịch với `g++ -std=c++17 main.cpp -o test -lcatch2` và chạy `./test`. Đầu ra mẫu:

```
Tất cả các bài kiểm tra đã vượt qua (1 tuyên bố trong 1 trường hợp kiểm tra)
```

## Sâu hơn
Việc kiểm tra không phải lúc nào cũng là quy chuẩn. Trong những năm '70, nó được thực hiện bằng tay. Giờ đây, các bài kiểm tra tự động là chìa khóa trong phương pháp agile và TDD (Phát triển dựa trên kiểm tra). Các lựa chọn thay thế cho Catch2? Google Test, Boost.Test, và CppUnit, mỗi cái có những đặc điểm riêng biệt. Nhớ rằng: các bài kiểm tra đánh giá xem mã có đáp ứng được yêu cầu hay không, không phải là yêu cầu đó có chính xác hay không—đó là vấn đề của spec.

## Xem thêm
- Catch2: https://github.com/catchorg/Catch2
- Google Test: https://github.com/google/googletest
- Boost.Test: https://www.boost.org/doc/libs/release/libs/test/
- CppUnit: https://freedesktop.org/wiki/Software/cppunit/
