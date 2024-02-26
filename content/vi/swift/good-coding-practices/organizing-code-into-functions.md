---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:40.659518-07:00
description: "Nh\xF3m m\xE3 l\u1EC7nh th\xE0nh c\xE1c h\xE0m l\xE0 vi\u1EC7c chia\
  \ nh\u1ECF c\xE1c nhi\u1EC7m v\u1EE5 th\xE0nh c\xE1c \u0111o\u1EA1n c\xF3 th\u1EC3\
  \ t\xE1i s\u1EED d\u1EE5ng. \u0110i\u1EC1u n\xE0y l\xE0m cho m\xE3 l\u1EC7nh tr\u1EDF\
  \ n\xEAn g\u1ECDn g\xE0ng, \xEDt l\u1ED7i v\xE0 d\u1EC5 debug\u2026"
lastmod: '2024-02-25T18:49:35.450966-07:00'
model: gpt-4-0125-preview
summary: "Nh\xF3m m\xE3 l\u1EC7nh th\xE0nh c\xE1c h\xE0m l\xE0 vi\u1EC7c chia nh\u1ECF\
  \ c\xE1c nhi\u1EC7m v\u1EE5 th\xE0nh c\xE1c \u0111o\u1EA1n c\xF3 th\u1EC3 t\xE1\
  i s\u1EED d\u1EE5ng. \u0110i\u1EC1u n\xE0y l\xE0m cho m\xE3 l\u1EC7nh tr\u1EDF n\xEA\
  n g\u1ECDn g\xE0ng, \xEDt l\u1ED7i v\xE0 d\u1EC5 debug\u2026"
title: "S\u1EAFp x\u1EBFp m\xE3 th\xE0nh c\xE1c h\xE0m"
---

{{< edit_this_page >}}

## Gì và Tại sao?
Nhóm mã lệnh thành các hàm là việc chia nhỏ các nhiệm vụ thành các đoạn có thể tái sử dụng. Điều này làm cho mã lệnh trở nên gọn gàng, ít lỗi và dễ debug hoặc cải tiến hơn.

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
