---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:00.852741-07:00
description: "L\xE0m Th\u1EBF N\xE0o: Swift l\xE0m cho vi\u1EC7c vi\u1EBFt hoa chu\u1ED7\
  i k\xFD t\u1EF1 tr\u1EDF n\xEAn tr\u1EF1c ti\u1EBFp. D\u01B0\u1EDBi \u0111\xE2y\
  \ l\xE0 m\u1ED9t h\u01B0\u1EDBng d\u1EABn nhanh."
lastmod: '2024-03-13T22:44:37.074326-06:00'
model: gpt-4-0125-preview
summary: "Swift l\xE0m cho vi\u1EC7c vi\u1EBFt hoa chu\u1ED7i k\xFD t\u1EF1 tr\u1EDF\
  \ n\xEAn tr\u1EF1c ti\u1EBFp."
title: "Vi\u1EBFt hoa m\u1ED9t chu\u1ED7i"
weight: 2
---

## Làm Thế Nào:
Swift làm cho việc viết hoa chuỗi ký tự trở nên trực tiếp. Dưới đây là một hướng dẫn nhanh:

```Swift
let lowercasedString = "hello, world!"
let titleCased = lowercasedString.capitalized // "Hello, World!"
let uppercasedString = lowercasedString.uppercased() // "HELLO, WORLD!"

// Kết Quả Mẫu:
print(titleCased)  // In ra "Hello, World!"
print(uppercasedString)  // In ra "HELLO, WORLD!"
```

Để kiểm soát nhiều hơn, chúng ta sẽ tiếp cận với `Locale`:

```Swift
let sentence = "the quick brown fox"
let titleCasedWithLocale = sentence.capitalized(with: Locale(identifier: "en_US"))
// "The Quick Brown Fox"

// Kết Quả Mẫu:
print(titleCasedWithLocale)  // In ra "The Quick Brown Fox"
```

## Sâu Hơn
Việc viết hoa trong lập trình đã tồn tại từ khi chúng ta có xử lý văn bản số hóa - nó tất cả là về việc đáp ứng kỳ vọng của người dùng. Trong khi `capitalized` trong Swift chuẩn hóa chuỗi thành Dạng Tiêu Đề, nơi ký tự đầu tiên của mỗi từ là chữ in hoa, có những sắc thái.

Trong quá khứ, các lập trình viên cần phải tự mình xử lý các phương thức tùy chỉnh để viết hoa, đối mặt với các trường hợp ngoại lệ một cách chủ quan. `capitalized` của Swift tính đến địa phương, điều này quan trọng đối với tên riêng hoặc quy tắc viết hoa cụ thể cho từng địa phương.

Nói về các lựa chọn thay thế, những người không hài lòng với `capitalized` thường chuyển sang regex hoặc viết phần mở rộng cho `String` để xử lý các quy tắc phức tạp hơn. Về mặt thực hiện, `capitalized` cơ bản là một phương thức tích hợp sẵn mà lặp qua chuỗi, áp dụng viết hoa cho chữ cái đầu tiên sau một ký tự không phải là chữ.

```Swift
extension String {
    func customCapitalized() -> String {
        return self.lowercased().replacingOccurrences(of: "\\b\\w", with: { 
            guard let firstChar = $0.first else { return $0 }
            return String(firstChar).uppercased() + $0.dropFirst()
        }, options: .regularExpression)
    }
}
```

Phần mở rộng trên sử dụng biểu thức chính quy để viết hoa chữ cái đầu tiên của mỗi từ.

## Xem Thêm
Để hiểu sâu hơn về việc thao tác chuỗi trong Swift, dưới đây là một số nguồn tài liệu hữu ích:
- [Tài liệu Swift về Chuỗi](https://developer.apple.com/documentation/swift/string)
- [Hướng Dẫn về Chuỗi trong Swift của Ray Wenderlich](https://www.raywenderlich.com/5492-working-with-strings-in-swift)
