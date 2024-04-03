---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:56.389195-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Swift l\xE0m cho vi\u1EC7c n\u1ED9i suy\
  \ chu\u1ED7i tr\u1EDF n\xEAn d\u1EC5 d\xE0ng v\u1EDBi c\xFA ph\xE1p `\\(t\xEAnBi\u1EBF\
  n)`."
lastmod: '2024-03-13T22:44:37.078280-06:00'
model: gpt-4-0125-preview
summary: "Swift l\xE0m cho vi\u1EC7c n\u1ED9i suy chu\u1ED7i tr\u1EDF n\xEAn d\u1EC5\
  \ d\xE0ng v\u1EDBi c\xFA ph\xE1p `\\(t\xEAnBi\u1EBFn)`."
title: "N\u1ED9i suy chu\u1ED7i k\xFD t\u1EF1"
weight: 8
---

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
