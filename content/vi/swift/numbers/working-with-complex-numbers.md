---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:27.223409-07:00
description: "S\u1ED1 ph\u1EE9c c\xF3 m\u1ED9t ph\u1EA7n th\u1EF1c v\xE0 m\u1ED9t\
  \ ph\u1EA7n \u1EA3o (nh\u01B0 3 + 4i). L\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5\
  ng ch\xFAng trong Swift cho c\xE1c nhi\u1EC7m v\u1EE5 nh\u01B0 x\u1EED l\xFD t\xED\
  n hi\u1EC7u, gi\u1EA3i quy\u1EBFt m\u1ED9t s\u1ED1 v\u1EA5n\u2026"
lastmod: 2024-02-19 22:04:56.294905
model: gpt-4-0125-preview
summary: "S\u1ED1 ph\u1EE9c c\xF3 m\u1ED9t ph\u1EA7n th\u1EF1c v\xE0 m\u1ED9t ph\u1EA7\
  n \u1EA3o (nh\u01B0 3 + 4i). L\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng ch\xFA\
  ng trong Swift cho c\xE1c nhi\u1EC7m v\u1EE5 nh\u01B0 x\u1EED l\xFD t\xEDn hi\u1EC7\
  u, gi\u1EA3i quy\u1EBFt m\u1ED9t s\u1ED1 v\u1EA5n\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi s\u1ED1 ph\u1EE9c"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Số phức có một phần thực và một phần ảo (như 3 + 4i). Lập trình viên sử dụng chúng trong Swift cho các nhiệm vụ như xử lý tín hiệu, giải quyết một số vấn đề toán học, và mô phỏng vật lý.

## Làm thế nào:
Swift không hỗ trợ số phức một cách sẵn có, nhưng chúng ta có thể tự lập trình:

```Swift
struct ComplexNumber {
    var real: Double
    var imaginary: Double
    
    func add(_ other: ComplexNumber) -> ComplexNumber {
        return ComplexNumber(real: real + other.real, imaginary: imaginary + other.imaginary)
    }
    
    // Các phương pháp khác như trừ, nhân, v.v.
}

let first = ComplexNumber(real: 2, imaginary: 3)
let second = ComplexNumber(real: 1, imaginary: 4)
let result = first.add(second)
print("Kết quả: \(result.real) + \(result.imaginary)i")
// Kết quả mẫu: Kết quả: 3.0 + 7.0i
```

## Sâu hơn
Số phức xuất hiện vào thế kỷ 16 trong các phương trình đại số. Chúng là yếu tố thiết yếu trong cơ học lượng tử, lý thuyết điều khiển, và nhiều lĩnh vực khác. Swift của Apple không có thư viện tiêu chuẩn cho số phức, không giống như các ngôn ngữ như Python hay C++. Các lựa chọn thay thế để tự lập trình bao gồm sử dụng gói Numerics bao gồm hỗ trợ số phức hoặc đóng gói thư viện phức của C++ với khả năng tương tác của Swift.

## Xem thêm
- Swift Numerics: [https://github.com/apple/swift-numerics](https://github.com/apple/swift-numerics)
