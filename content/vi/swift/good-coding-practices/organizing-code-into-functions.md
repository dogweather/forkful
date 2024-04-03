---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:40.659518-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: H\xE3y t\u01B0\u1EDFng t\u01B0\u1EE3ng\
  \ m\u1ED9t nhi\u1EC7m v\u1EE5: t\xEDnh trung b\xECnh c\u1EE7a m\u1ED9t m\u1EA3ng.\
  \ Kh\xF4ng d\xF9ng h\xE0m, b\u1EA1n s\u1EBD \u0111\u01B0a t\u1EA5t c\u1EA3 v\xE0\
  o main. D\xF9ng h\xE0m, b\u1EA1n l\xE0m nh\u01B0 sau."
lastmod: '2024-03-13T22:44:37.104853-06:00'
model: gpt-4-0125-preview
summary: "H\xE3y t\u01B0\u1EDFng t\u01B0\u1EE3ng m\u1ED9t nhi\u1EC7m v\u1EE5."
title: "S\u1EAFp x\u1EBFp m\xE3 th\xE0nh c\xE1c h\xE0m"
weight: 18
---

## Cách thực hiện:
Hãy tưởng tượng một nhiệm vụ: tính trung bình của một mảng. Không dùng hàm, bạn sẽ đưa tất cả vào main. Dùng hàm, bạn làm như sau:

```swift
func calculateAverage(of numbers: [Double]) -> Double {
    let sum = numbers.reduce(0, +)
    return numbers.isEmpty ? 0 : sum / Double(numbers.count)
}

// Cách sử dụng
let scores = [92.5, 88.75, 99.0, 70.5]
let averageScore = calculateAverage(of: scores)
print("Điểm trung bình là \(averageScore)")
```

Kết quả mẫu sẽ là: 
```
Điểm trung bình là 87.6875
```

## Đào sâu
Theo lịch sử, khi lập trình ngày càng phức tạp, hàm trở thành một yếu tố quan trọng trong việc quản lý độ phức tạp. Các phương pháp thay thế bao gồm việc ghi mã trực tiếp và sao chép dán mã (mã spaghetti) – nay được coi là thực hành xấu. Trong Swift, hàm được xem là công dân hạng nhất; chúng có thể được gán cho biến, truyền dưới dạng đối số, và được trả về từ các hàm khác, làm cho mã lệnh trở nên linh hoạt và mô-đun hơn.

Về mặt triển khai, hãy thiết kế các hàm của bạn để thực hiện một việc tốt. Hãy mục tiêu cho các hàm có một mục đích rõ ràng và một tên phản ánh điều đó. Chú ý đến số lượng tham số — quá nhiều và bạn có thể đang làm quá nhiều. Xử lý lỗi? Cân nhắc tới việc sử dụng các hàm ném lỗi và xử lý vấn đề một cách thanh lịch. Nhớ rằng: Swift đề cao tính dễ đọc và dễ bảo trì.

## Xem thêm
- [Hướng dẫn Lập trình Ngôn ngữ Swift - Hàm](https://docs.swift.org/swift-book/LanguageGuide/Functions.html)
- [Hướng dẫn Phong cách Swift của Ray Wenderlich](https://github.com/raywenderlich/swift-style-guide)
- [Refactoring: Cải thiện Thiết kế của Mã Lệnh Đang Tồn tại của Martin Fowler](https://martinfowler.com/books/refactoring.html)
