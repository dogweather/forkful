---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:22.312274-07:00
description: "Vi\u1EBFt test l\xE0 vi\u1EC7c t\u1EA1o ra code \u0111\u1EC3 ki\u1EC3\
  m tra xem ph\u1EA7n m\u1EC1m c\u1EE7a b\u1EA1n c\xF3 ho\u1EA1t \u0111\u1ED9ng nh\u01B0\
  \ \u0111\xE3 d\u1EF1 \u0111\u1ECBnh hay kh\xF4ng. L\u1EADp tr\xECnh vi\xEAn th\u1EF1\
  c hi\u1EC7n test \u0111\u1EC3 b\u1EAFt l\u1ED7i s\u1EDBm, \u0111\u1EA3m\u2026"
lastmod: '2024-03-13T22:44:37.101991-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EBFt test l\xE0 vi\u1EC7c t\u1EA1o ra code \u0111\u1EC3 ki\u1EC3m tra\
  \ xem ph\u1EA7n m\u1EC1m c\u1EE7a b\u1EA1n c\xF3 ho\u1EA1t \u0111\u1ED9ng nh\u01B0\
  \ \u0111\xE3 d\u1EF1 \u0111\u1ECBnh hay kh\xF4ng."
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
