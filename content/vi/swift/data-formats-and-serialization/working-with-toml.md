---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:16.875952-07:00
description: "L\xE0m th\u1EBF n\xE0o: \u0110\u1EC3 b\u1EAFt \u0111\u1EA7u, b\u1EA1\
  n c\u1EA7n m\u1ED9t b\u1ED9 ph\xE2n t\xEDch TOML. Swift kh\xF4ng c\xF3 s\u1EB5n\
  \ m\u1ED9t b\u1ED9 ph\xE2n t\xEDch, v\u1EADy n\xEAn ch\xFAng ta s\u1EBD s\u1EED\
  \ d\u1EE5ng `TOMLDecoder`. C\xE0i \u0111\u1EB7t n\xF3 qua\u2026"
lastmod: '2024-03-13T22:44:37.128411-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1EC3 b\u1EAFt \u0111\u1EA7u, b\u1EA1n c\u1EA7n m\u1ED9t b\u1ED9\
  \ ph\xE2n t\xEDch TOML."
title: "L\xE0m vi\u1EC7c v\u1EDBi TOML"
weight: 39
---

## Làm thế nào:
Để bắt đầu, bạn cần một bộ phân tích TOML. Swift không có sẵn một bộ phân tích, vậy nên chúng ta sẽ sử dụng `TOMLDecoder`. Cài đặt nó qua Swift Package Manager và sau đó tuần tự hóa và bỏ tuần tự TOML một cách dễ dàng.

```Swift
import TOMLDecoder

let tomlString = """
title = "Ví dụ TOML"

[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z
"""

struct Config: Codable {
    let title: String
    let owner: Owner
}

struct Owner: Codable {
    let name: String
    let dob: Date
}

let decoder = TOMLDecoder()
if let configData = tomlString.data(using: .utf8) {
    do {
        let config = try decoder.decode(Config.self, from: configData)
        print("Tiêu đề: \(config.title), Chủ sở hữu: \(config.owner.name), Ngày sinh: \(config.owner.dob)")
    } catch {
        print("Lỗi phân tích TOML: \(error)")
    }
}
```

Đoạn mã này khi chạy sẽ cho kết quả:
```
Tiêu đề: Ví dụ TOML, Chủ sở hữu: Tom Preston-Werner, Ngày sinh: 1979-05-27 07:32:00 +0000
```

## Sâu hơn
TOML được thiết kế bởi Tom Preston-Werner, đồng sáng lập GitHub, như một lựa chọn thân thiện với con người hơn so với các định dạng như JSON hay YAML. Nó hướng tới rõ ràng, giảm khả năng hiểu nhầm bởi con người hoặc máy móc. Về các lựa chọn khác, YAML và JSON là những ứng cử viên thông thường, với YAML nghiêng về khả năng đọc bởi con người và JSON là lựa chọn thân thiện với máy móc đơn giản hơn. Khi làm việc với TOML trong Swift, chúng ta không có bộ phân tích gốc. Tuy nhiên, thư viện bên thứ ba như `TOMLDecoder` tạo điều kiện chuyển đổi dễ dàng giữa chuỗi TOML và các loại Swift, cụ thể là qua giao thức `Codable` được giới thiệu trong Swift 4 đã đơn giản hóa việc tuần tự hóa.

## Xem Thêm
- Tiêu chuẩn TOML: https://toml.io
- GitHub cho `TOMLDecoder`: https://github.com/dduan/TOMLDecoder
- Tài liệu Swift về `Codable`: https://developer.apple.com/documentation/swift/codable
- So sánh các định dạng tuần tự hóa dữ liệu: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
