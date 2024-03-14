---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:04.076707-07:00
description: "Bi\u1EC3u th\u1EE9c ch\xEDnh quy, ho\u1EB7c regex, l\xE0 nh\u1EEFng\
  \ m\u1EABu \u0111\u01B0\u1EE3c s\u1EED d\u1EE5ng \u0111\u1EC3 t\xECm k\u1EBFt h\u1EE3\
  p c\xE1c k\xFD t\u1EF1 trong chu\u1ED7i. C\xE1c l\u1EADp tr\xECnh vi\xEAn s\u1EED\
  \ d\u1EE5ng ch\xFAng \u0111\u1EC3 t\xECm ki\u1EBFm, ch\u1EC9nh s\u1EEDa,\u2026"
lastmod: '2024-03-13T22:44:37.083551-06:00'
model: gpt-4-0125-preview
summary: "Bi\u1EC3u th\u1EE9c ch\xEDnh quy, ho\u1EB7c regex, l\xE0 nh\u1EEFng m\u1EAB\
  u \u0111\u01B0\u1EE3c s\u1EED d\u1EE5ng \u0111\u1EC3 t\xECm k\u1EBFt h\u1EE3p c\xE1\
  c k\xFD t\u1EF1 trong chu\u1ED7i. C\xE1c l\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5\
  ng ch\xFAng \u0111\u1EC3 t\xECm ki\u1EBFm, ch\u1EC9nh s\u1EEDa,\u2026"
title: "S\u1EED d\u1EE5ng bi\u1EC3u th\u1EE9c ch\xEDnh quy"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Biểu thức chính quy, hoặc regex, là những mẫu được sử dụng để tìm kết hợp các ký tự trong chuỗi. Các lập trình viên sử dụng chúng để tìm kiếm, chỉnh sửa, hoặc kiểm tra văn bản, làm cho các nhiệm vụ liên quan đến thao tác chuỗi trở nên hiệu quả hơn và ít lỗi hơn.

## Làm thế nào:
Trong Swift, bạn sử dụng lớp `NSRegularExpression` để xử lý regex. Bạn xác định một mẫu, tạo một đối tượng regex, và sau đó sử dụng nó để tìm kiếm hoặc thay thế văn bản. Dưới đây là một ví dụ cơ bản:

```Swift
import Foundation

let input = "Call me at 555-1234 or 555-5678."
let pattern = "\\d{3}-\\d{4}" // Khớp với mẫu như 555-1234

do {
    let regex = try NSRegularExpression(pattern: pattern)
    let matches = regex.matches(in: input, range: NSRange(input.startIndex..., in: input))
    
    for match in matches {
        if let range = Range(match.range, in: input) {
            let phoneNumber = String(input[range])
            print("Đã tìm thấy số điện thoại: \(phoneNumber)")
        }
    }
} catch {
    print("Lỗi Regex: \(error.localizedDescription)")
}
```

Kết quả mẫu:
```
Đã tìm thấy số điện thoại: 555-1234
Đã tìm thấy số điện thoại: 555-5678
```

## Sâu hơn
Regex đã tồn tại từ những năm 1950, xuất phát từ lý thuyết ngôn ngữ hình thức và trở nên phổ biến trong các công cụ Unix. Trong Swift, chúng ta sử dụng lớp `NSRegularExpression` kế thừa từ Objective-C, dựa vào thư viện ICU để hỗ trợ Unicode.

Các phương án thay thế cho regex trong Swift bao gồm sử dụng các phương thức `contains`, `split`, hoặc `range(of:)` của `String` cho các trường hợp đơn giản. Đối với việc khớp mẫu phức tạp hơn, Swift không cung cấp các phương án thay thế có sẵn cho regex.

Khi triển khai regex, điều quan trọng là tối ưu hóa mẫu để tránh việc tìm kiếm chậm, đặc biệt là với các văn bản lớn. Ngoài ra, hãy nhớ rằng các thao tác regex có thể gây ra ngoại lệ, vì vậy luôn xử lý chúng với các khối `try-catch`.

## Xem thêm
- [Tài liệu NSRegularExpression](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Tài liệu Chuỗi Swift](https://developer.apple.com/documentation/swift/string)
- [Hướng dẫn NSRegularExpression trong Swift của Ray Wenderlich](https://www.raywenderlich.com/2725-nsregularexpression-tutorial-and-cheat-sheet)
