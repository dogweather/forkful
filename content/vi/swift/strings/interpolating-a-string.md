---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:56.389195-07:00
description: "N\u1ED9i suy chu\u1ED7i bao g\u1ED3m vi\u1EC7c ch\xE8n c\xE1c bi\u1EBF\
  n v\xE0o trong m\u1ED9t chu\u1ED7i k\xFD t\u1EF1. L\u1EADp tr\xECnh vi\xEAn th\u1EF1\
  c hi\u1EC7n \u0111i\u1EC1u n\xE0y \u0111\u1EC3 t\u1EA1o ra chu\u1ED7i m\u1ED9t c\xE1\
  ch \u0111\u1ED9ng, l\xE0m cho vi\u1EC7c bao g\u1ED3m\u2026"
lastmod: '2024-03-13T22:44:37.078280-06:00'
model: gpt-4-0125-preview
summary: "N\u1ED9i suy chu\u1ED7i bao g\u1ED3m vi\u1EC7c ch\xE8n c\xE1c bi\u1EBFn\
  \ v\xE0o trong m\u1ED9t chu\u1ED7i k\xFD t\u1EF1. L\u1EADp tr\xECnh vi\xEAn th\u1EF1\
  c hi\u1EC7n \u0111i\u1EC1u n\xE0y \u0111\u1EC3 t\u1EA1o ra chu\u1ED7i m\u1ED9t c\xE1\
  ch \u0111\u1ED9ng, l\xE0m cho vi\u1EC7c bao g\u1ED3m\u2026"
title: "N\u1ED9i suy chu\u1ED7i k\xFD t\u1EF1"
weight: 8
---

## Gì và Tại sao?
Nội suy chuỗi bao gồm việc chèn các biến vào trong một chuỗi ký tự. Lập trình viên thực hiện điều này để tạo ra chuỗi một cách động, làm cho việc bao gồm dữ liệu biến trong đầu ra trở nên dễ dàng hơn.

## Cách thực hiện:
Swift làm cho việc nội suy chuỗi trở nên dễ dàng với cú pháp `\(tênBiến)`.

```Swift
let name = "Jane"
let age = 28
let greeting = "Xin chào, \(name), bạn \(age) tuổi."
print(greeting)  // Đầu ra: Xin chào, Jane, bạn 28 tuổi.
```

Bạn thậm chí còn có thể thực hiện các phép toán trong quá trình nội suy:

```Swift
let apples = 3
let oranges = 5
let fruitSummary = "Tôi có \(apples + oranges) quả trái cây."
print(fruitSummary)  // Đầu ra: Tôi có 8 quả trái cây.
```

## Sâu hơn
Được rồi, hãy cùng nhìn lại một chút lịch sử. Nội suy chuỗi không phải là độc quyền của Swift. Nó tồn tại trong nhiều ngôn ngữ (như JavaScript, Python, v.v.), nhưng phiên bản của Swift là an toàn về kiểu dữ liệu, nghĩa là trình biên dịch sẽ kiểm tra các kiểu dữ liệu cho bạn, giảm bớt lỗi.

Trước Swift 5, nội suy chuỗi kém mạnh mẽ và cồng kềnh hơn. Nhưng Swift 5 đã giới thiệu Nội suy Chuỗi Mở rộng, cho phép bạn tùy chỉnh nội suy chuỗi, mang lại sự linh hoạt ấn tượng.

Các phương pháp thay thế cho nội suy chuỗi trong Swift bao gồm ghép nối bằng `+`, và phương pháp cũ `String(format:)`. Tuy nhiên, những phương pháp này kém tiện lợi hơn và, đối với các chuỗi định dạng, khó đọc hơn.

Chi tiết thực hiện? Với nội suy chuỗi của Swift, bạn có thể tùy chỉnh cách các kiểu dữ liệu được biểu diễn trong chuỗi bằng cách mở rộng giao thức `StringInterpolation`. Điều này có nghĩa là bạn có thể xác định cách hiển thị các kiểu tùy chỉnh trong quá trình nội suy, điều này rất tiện lợi.

```Swift
extension String.StringInterpolation {
    mutating func appendInterpolation(_ value: Date) {
        let formatter = DateFormatter()
        formatter.dateStyle = .medium
        appendLiteral(formatter.string(from: value))
    }
}

let today = Date()
let dateString = "Ngày hôm nay là \(today)."
print(dateString) // Đầu ra sẽ là ngày hôm nay với định dạng kiểu trung bình.
```

## Xem thêm
Để hiểu rõ hơn về nội suy chuỗi, tài liệu của Swift rất hữu ích:
- [Nội suy Chuỗi](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID292)
- [Đề xuất Cải tiến Nội suy Chuỗi trong Swift từ Swift Evolution](https://github.com/apple/swift-evolution/blob/main/proposals/0228-fix-expressiblebystringinterpolation.md)

Để đào sâu hơn vào định dạng các kiểu tùy chỉnh:
- [Tùy chỉnh Nội suy Chuỗi trong Swift](https://www.hackingwithswift.com/articles/178/super-powered-string-interpolation-in-swift-5)
