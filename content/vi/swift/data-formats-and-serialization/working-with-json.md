---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:42.851699-07:00
description: "L\xE0m th\u1EBF n\xE0o: Swift l\xE0m cho vi\u1EC7c ph\xE2n t\xEDch JSON\
  \ tr\u1EDF n\xEAn d\u1EC5 d\xE0ng v\u1EDBi giao th\u1EE9c `Codable`. D\u01B0\u1EDB\
  i \u0111\xE2y l\xE0 c\xE1ch b\u1EA1n gi\u1EA3i m\xE3 JSON th\xE0nh m\u1ED9t \u0111\
  \u1ED1i t\u01B0\u1EE3ng Swift."
lastmod: '2024-03-13T22:44:37.125953-06:00'
model: gpt-4-0125-preview
summary: "Swift l\xE0m cho vi\u1EC7c ph\xE2n t\xEDch JSON tr\u1EDF n\xEAn d\u1EC5\
  \ d\xE0ng v\u1EDBi giao th\u1EE9c `Codable`."
title: "L\xE0m vi\u1EC7c v\u1EDBi JSON"
weight: 38
---

## Làm thế nào:
Swift làm cho việc phân tích JSON trở nên dễ dàng với giao thức `Codable`. Dưới đây là cách bạn giải mã JSON thành một đối tượng Swift:

```Swift
import Foundation

// Định nghĩa một mô hình tuân thủ Codable
struct User: Codable {
    var name: String
    var age: Int
}

// Chuỗi JSON
let jsonString = """
{
    "name": "John Doe",
    "age": 30
}
"""

// Chuyển đổi chuỗi JSON thành Data
if let jsonData = jsonString.data(using: .utf8) {
    // Giải mã dữ liệu JSON thành đối tượng User
    do {
        let user = try JSONDecoder().decode(User.self, from: jsonData)
        print("Tên: \(user.name), Tuổi: \(user.age)")
    } catch {
        print("Lỗi giải mã JSON: \(error)")
    }
}
```

Kết quả mẫu:
```
Tên: John Doe, Tuổi: 30
```

## Sâu hơn nữa
JSON (JavaScript Object Notation) đã được áp dụng rộng rãi kể từ đầu những năm 2000, sau khi Douglas Crockford chỉ ra nó. Nó đã thay thế XML cho nhiều trường hợp sử dụng vì cú pháp đơn giản hơn và hiệu suất tốt hơn. Mặc dù `Codable` của Swift là lựa chọn hàng đầu cho JSON, những phương án khác như `JSONSerialization` tồn tại cho khi đối mặt với các loại không tuân thủ Codable. Về bản chất, `Codable` che giấu đi việc phân tích cú pháp cấp thấp và làm cho việc serial hóa/deserial hóa trở nên trơn tru.

## Xem thêm
- Khám phá thêm về JSON và Swift tại blog chính thức của Swift: [Swift.org](https://swift.org/blog/)
- Kiểm tra tài liệu `Codable`: [Swift Codable](https://developer.apple.com/documentation/swift/codable)
- Đối với các cấu trúc JSON phức tạp, cân nhắc sử dụng các thư viện bên thứ ba như SwiftyJSON có sẵn trên [GitHub](https://github.com/SwiftyJSON/SwiftyJSON).
