---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:22.312274-07:00
description: "L\xE0m th\u1EBF n\xE0o: Swift s\u1EED d\u1EE5ng b\u1ED9 khung XCTest\
  \ \u0111\u1EC3 th\u1EF1c hi\u1EC7n test. D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9\
  t b\xE0i test \u0111\u01A1n gi\u1EA3n cho h\xE0m `add(a:b:)`."
lastmod: '2024-03-13T22:44:37.101991-06:00'
model: gpt-4-0125-preview
summary: "Swift s\u1EED d\u1EE5ng b\u1ED9 khung XCTest \u0111\u1EC3 th\u1EF1c hi\u1EC7\
  n test."
title: "Vi\u1EBFt c\xE1c b\xE0i ki\u1EC3m tra"
weight: 36
---

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
