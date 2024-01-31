---
title:                "Sắp xếp mã thành các hàm"
date:                  2024-01-28T22:03:40.659518-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sắp xếp mã thành các hàm"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/swift/organizing-code-into-functions.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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
