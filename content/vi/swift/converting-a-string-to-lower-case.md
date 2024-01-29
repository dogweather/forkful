---
title:                "Chuyển đổi chuỗi thành chữ thường"
date:                  2024-01-28T21:58:59.871868-07:00
model:                 gpt-4-0125-preview
simple_title:         "Chuyển đổi chuỗi thành chữ thường"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/swift/converting-a-string-to-lower-case.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Chuyển đổi một chuỗi thành chữ thường có nghĩa là thay đổi bất kỳ chữ cái viết hoa nào thành các chữ cái viết thường tương ứng. Lập trình viên thực hiện việc này để đạt được tính nhất quán, thường xuyên dùng cho việc so sánh không phân biệt chữ hoa chữ thường hoặc chuẩn hóa nhập liệu văn bản.

## Cách làm:

Swift làm điều này trở nên dễ dàng với một thuộc tính gọi là `lowercased`. Dưới đây là cách bạn sử dụng nó:

```Swift
let originalString = "Hello, World!"
let lowercasedString = originalString.lowercased()
print(lowercasedString) // "hello, world!"
```

Kết quả mẫu:
```
hello, world!
```

## Đào sâu:

Trong lịch sử, việc đảm bảo sự nhất quán của chữ cái trong chuỗi luôn rất quan trọng trong lập trình, chủ yếu vì máy tính đầu tiên rất nhạy cảm với việc phân biệt chữ hoa chữ thường. Trong Swift, `lowercased()` là một phương thức có sẵn trên các thể hiện của kiểu `String`. Bằng cách gọi nó, bạn chuyển đổi tất cả các ký tự trong chuỗi có các biến thể viết thường thành hình thức viết thường của chúng.

Các phương án thay thế cho `lowercased()` có thể là duyệt qua chuỗi một cách thủ công và thay thế mỗi ký tự bằng tương đương viết thường của nó bằng cách sử dụng một hàm ánh xạ. Nhưng, thực sự, đó là việc tái tạo bánh xe.

Chuyển đổi chuỗi thành chữ thường có một số nét tinh tế. Ví dụ, phương thức `lowercased()` sử dụng ngữ cảnh địa phương hiện tại để xử lý các quy tắc viết hoa cụ thể của ngôn ngữ, điều này không phải lúc nào cũng là hành vi mong muốn. Nếu bạn cần thực hiện chuyển đổi không phụ thuộc vào ngữ cảnh địa phương, bạn có thể sử dụng `lowercased(with: Locale?)` và truyền `nil` cho Locale:

```Swift
let turkishString = "İstanbul"
let lowercasedTurkishString = turkishString.lowercased(with: nil)
print(lowercasedTurkishString) // "i̇stanbul", chính xác theo Unicode, nhưng 'I' không có dấu chấm có thể được mong đợi ở Thổ Nhĩ Kỳ.
```

Việc thực hiện của `lowercased()` ẩn dưới lớp vỏ tận dụng chuẩn Unicode, bao gồm các quy tắc ánh xạ phức tạp cho các ký tự trong nhiều bộ ký hiệu, không phải tất cả đều là chuyện đơn giản của việc thay 'A' bằng 'a'.

## Xem thêm:

Để khám phá thêm về chuỗi và chuyển đổi ký tự trong Swift, hãy tham khảo các nguồn tài nguyên sau:

- Tài liệu về Chuỗi và Ký tự Swift: [Swift.org](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- Chi tiết về ánh xạ chữ hoa/chữ thường Unicode: [Tiêu chuẩn Unicode](https://www.unicode.org/reports/tr21/tr21-5.html)
- Thảo luận về so sánh chuỗi và ngữ cảnh địa phương: [Bài viết của NSHipster về Ngữ cảnh địa phương](https://nshipster.com/locale/)
