---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:43.606792-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Swift kh\xF4ng c\xF3 kh\u1EA3 n\u0103ng\
  \ x\u1EED l\xFD YAML m\u1ED9t c\xE1ch t\u1EF1 nhi\xEAn, v\xEC v\u1EADy ch\xFAng\
  \ ta c\u1EA7n s\u1EED d\u1EE5ng m\u1ED9t th\u01B0 vi\u1EC7n b\xEAn th\u1EE9 ba nh\u01B0\
  \ Yams. \u0110\u1EA7u ti\xEAn, th\xEAm Yams\u2026"
lastmod: '2024-03-13T22:44:37.124664-06:00'
model: gpt-4-0125-preview
summary: "Swift kh\xF4ng c\xF3 kh\u1EA3 n\u0103ng x\u1EED l\xFD YAML m\u1ED9t c\xE1\
  ch t\u1EF1 nhi\xEAn, v\xEC v\u1EADy ch\xFAng ta c\u1EA7n s\u1EED d\u1EE5ng m\u1ED9\
  t th\u01B0 vi\u1EC7n b\xEAn th\u1EE9 ba nh\u01B0 Yams."
title: "L\xE0m vi\u1EC7c v\u1EDBi YAML"
weight: 41
---

## Cách thực hiện:
Swift không có khả năng xử lý YAML một cách tự nhiên, vì vậy chúng ta cần sử dụng một thư viện bên thứ ba như Yams. Đầu tiên, thêm Yams vào `Package.swift` của bạn:

```swift
dependencies: [
    .package(url: "https://github.com/jpsim/Yams.git", from: "4.0.0")
]
```

Sau đó, nhập Yams và sử dụng nó để phân tích YAML thành một dictionary Swift:

```swift
import Yams

let yamlString = """
name: John Doe
age: 34
languages:
  - Swift
  - Python
"""

do {
    if let data = try Yams.load(yaml: yamlString) as? [String: Any] {
        print(data)
    }
} catch {
    print("Không thể phân tích chuỗi YAML.")
}

// Đầu ra:
// ["name": "John Doe", "age": 34, "languages": ["Swift", "Python"]]
```

Nếu bạn muốn sinh ra YAML từ đối tượng Swift:

```swift
import Yams

let dictionary: [String: Any] = [
    "name": "Jane Smith",
    "age": 28,
    "languages": ["Java", "Kotlin"]
]

do {
    let yaml = try Yams.dump(object: dictionary)
    print(yaml)
} catch {
    print("Không thể chuyển dictionary thành YAML.")
}

// Đầu ra:
// age: 28
// languages:
//   - Java
//   - Kotlin
// name: Jane Smith
```

## Sâu hơn
YAML xuất phát từ năm 2001 như một lựa chọn thân thiện với con người thay thế cho XML. Nó giống với JSON nhưng sử dụng ít ngoặc và dễ đọc hơn cho con người. Trong khi JSON là lựa chọn hàng đầu cho các API web, YAML được ưa chuộng cho các tệp cấu hình. Các lựa chọn thay thế bao gồm TOML và JSON5, nhưng việc sử dụng khoảng trắng của YAML và khả năng bình luận các dòng khiến nó trở nên đáng mong muốn. Với Yams, Swift tiếp cận việc xử lý YAML với việc ánh xạ lớp, cung cấp sự cân bằng giữa sự đơn giản giống như kịch bản và an toàn kiểu.

## Xem thêm
- Trang chính thức của YAML để biết chi tiết về spec: [https://yaml.org](https://yaml.org)
- Kho lưu trữ GitHub của Yams: [https://github.com/jpsim/Yams](https://github.com/jpsim/Yams)
- Tài liệu về Quản lý Gói Swift: [https://swift.org/package-manager/](https://swift.org/package-manager/)
