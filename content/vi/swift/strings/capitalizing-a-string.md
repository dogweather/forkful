---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:00.852741-07:00
description: "Vi\u1EC7c vi\u1EBFt hoa c\xF3 ngh\u0129a l\xE0 thay \u0111\u1ED5i ch\u1EEF\
  \ c\xE1i \u0111\u1EA7u ti\xEAn c\u1EE7a t\u1EEB th\xE0nh ch\u1EEF in hoa; trong\
  \ chu\u1ED7i k\xFD t\u1EF1, th\u01B0\u1EDDng li\xEAn quan \u0111\u1EBFn vi\u1EC7\
  c \u0111\u1ECBnh d\u1EA1ng ho\u1EB7c l\xE0m cho v\u0103n b\u1EA3n\u2026"
lastmod: '2024-02-25T18:49:35.421020-07:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c vi\u1EBFt hoa c\xF3 ngh\u0129a l\xE0 thay \u0111\u1ED5i ch\u1EEF\
  \ c\xE1i \u0111\u1EA7u ti\xEAn c\u1EE7a t\u1EEB th\xE0nh ch\u1EEF in hoa; trong\
  \ chu\u1ED7i k\xFD t\u1EF1, th\u01B0\u1EDDng li\xEAn quan \u0111\u1EBFn vi\u1EC7\
  c \u0111\u1ECBnh d\u1EA1ng ho\u1EB7c l\xE0m cho v\u0103n b\u1EA3n\u2026"
title: "Vi\u1EBFt hoa m\u1ED9t chu\u1ED7i"
---

{{< edit_this_page >}}

## Cái Gì & Tại Sao?
Việc viết hoa có nghĩa là thay đổi chữ cái đầu tiên của từ thành chữ in hoa; trong chuỗi ký tự, thường liên quan đến việc định dạng hoặc làm cho văn bản thân thiện với người dùng hơn. Các lập trình viên viết hoa chuỗi ký tự để tăng tính dễ đọc, tuân thủ quy tắc ngữ pháp, hoặc phù hợp với hướng dẫn về phong cách.

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
