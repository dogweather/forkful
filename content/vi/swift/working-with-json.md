---
title:                "Làm việc với JSON"
date:                  2024-01-28T22:10:42.851699-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/swift/working-with-json.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?

Làm việc với JSON trong Swift nghĩa là đối phó với một định dạng dữ liệu gọn nhẹ dành cho trao đổi dữ liệu. Lập trình viên sử dụng JSON để truyền dữ liệu giữa máy chủ và ứng dụng web bởi vì nó dễ đọc và dễ phân tích đối với cả con người và máy móc.

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
