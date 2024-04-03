---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:27.223409-07:00
description: "L\xE0m th\u1EBF n\xE0o: Swift kh\xF4ng h\u1ED7 tr\u1EE3 s\u1ED1 ph\u1EE9\
  c m\u1ED9t c\xE1ch s\u1EB5n c\xF3, nh\u01B0ng ch\xFAng ta c\xF3 th\u1EC3 t\u1EF1\
  \ l\u1EADp tr\xECnh."
lastmod: '2024-03-13T22:44:37.088693-06:00'
model: gpt-4-0125-preview
summary: "Swift kh\xF4ng h\u1ED7 tr\u1EE3 s\u1ED1 ph\u1EE9c m\u1ED9t c\xE1ch s\u1EB5\
  n c\xF3, nh\u01B0ng ch\xFAng ta c\xF3 th\u1EC3 t\u1EF1 l\u1EADp tr\xECnh."
title: "L\xE0m vi\u1EC7c v\u1EDBi s\u1ED1 ph\u1EE9c"
weight: 14
---

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
