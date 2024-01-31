---
title:                "Viết các bài kiểm tra"
date:                  2024-01-28T22:13:22.312274-07:00
model:                 gpt-4-0125-preview
simple_title:         "Viết các bài kiểm tra"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/swift/writing-tests.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?
Viết test là việc tạo ra code để kiểm tra xem phần mềm của bạn có hoạt động như đã dự định hay không. Lập trình viên thực hiện test để bắt lỗi sớm, đảm bảo chất lượng và đơn giản hóa việc bảo trì.

## Làm thế nào:
Swift sử dụng bộ khung XCTest để thực hiện test. Dưới đây là một bài test đơn giản cho hàm `add(a:b:)`:

```Swift
import XCTest

class MathTests: XCTestCase {

    func testAdd() {
        let result = add(a: 2, b: 3)
        XCTAssertEqual(result, 5, "Kỳ vọng 2 + 3 bằng 5")
    }

    func add(a: Int, b: Int) -> Int {
        return a + b
    }
}
```
Chạy test bằng cách sử dụng Test Navigator của Xcode hoặc dùng `cmd+U`. Kết quả đầu ra nên hiển thị:

```plaintext
Bộ test 'Tất cả các test' đã vượt qua lúc ...
    Thực hiện 1 test, với 0 lỗi (0 không mong muốn) trong 0.001 (0.004) giây
```

## Đào sâu
XCTest, một phần của Xcode kể từ năm 2013, đã tiếp quản từ OCUnit. Các lựa chọn thay thế là Quick (bộ khung BDD) và SnapshotTesting (test giao diện người dùng). Việc thực hiện kiểm thử dựa vào các hàm xác định, các trường hợp test và tùy chọn các bộ test, tận dụng các khả năng của bộ khung XCTest.

## Xem thêm
- [Tổng quan về Apple XCTest](https://developer.apple.com/documentation/xctest)
- [Hướng dẫn test đơn vị và test giao diện người dùng iOS của Ray Wenderlich](https://www.raywenderlich.com/21020457-ios-unit-testing-and-ui-testing-tutorial)
- [Test mã Swift với Quick](https://github.com/Quick/Quick)
- [SnapshotTesting trên GitHub](https://github.com/pointfreeco/swift-snapshot-testing)
