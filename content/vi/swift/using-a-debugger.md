---
title:                "Sử dụng bộ gỡ lỗi"
date:                  2024-01-28T22:09:05.940464-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng bộ gỡ lỗi"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/swift/using-a-debugger.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại sao?
Sử dụng debugger có nghĩa là sử dụng các công cụ chuyên biệt để kiểm tra và kiểm soát mã của bạn khi nó chạy. Điều này thực sự quan trọng bởi vì nó cho phép bạn thấy những gì đang diễn ra bên dưới "nắp máy", tìm lỗi và hiểu rõ hơn về hành vi của mã.

## Làm thế nào:
Để sử dụng debugger trong Xcode (môi trường phát triển tích hợp dành cho Swift), bạn có thể đặt điểm dừng (breakpoints), kiểm tra biến và theo dõi biểu thức. Dưới đây là một ví dụ:

```Swift
func findFactorial(of number: Int) -> Int {
    if number == 0 {
        return 1
    }
    return number * findFactorial(of: number - 1)
}

let result = findFactorial(of: 5)
print(result)
```

Đặt một điểm dừng bằng cách nhấp vào bên trái một số dòng trong Xcode và chạy chương trình. Khi nó chạm vào điểm dừng, Xcode tạm dừng việc thực thi. Bây giờ bạn có thể:

1. Kiểm tra giá trị của biến.
2. Bước qua (chạy dòng tiếp theo) hoặc bước vào (vào bên trong một hàm) sử dụng các điều khiển debugger.
3. Thêm biểu thức vào 'danh sách theo dõi' để theo dõi sự thay đổi của các biến hoặc hằng số cụ thể.

Dưới đây là những gì bạn có thể thấy trong khu vực debug:

```
(lldb) po number
5
(lldb) po result
120
```

## Sâu hơn:
Debugger đã là một phần của cảnh quan lập trình từ những năm 1940, phát triển từ hệ thống điểm dừng đơn giản đến các trải nghiệm phức tạp, được điều khiển bằng giao diện người dùng. Các lựa chọn khác bên cạnh debugger tích hợp của Xcode bao gồm các công cụ bên thứ ba như LLDB (Low Level Debugger) mà Xcode sử dụng phía sau hậu trường. Một số người thậm chí còn debug bằng cách sử dụng lệnh `print()` (thường được biết đến với tên gọi "debug kiểu người nguyên thủy"), nhưng điều này kém hiệu quả hơn cho các dự án lớn hoặc lỗi phức tạp. Khi bạn sử dụng debugger, bạn đang thao túng điều khiển thực thi, kiểm tra runtime và thao tác dữ liệu. Một hiểu biết sâu sắc về những nguyên tắc này giúp rất nhiều trong việc debug hiệu quả.

## Xem thêm:
- [Hướng dẫn Debug Xcode của Apple](https://developer.apple.com/documentation/xcode/debugging-your-app)
- [Hướng dẫn Nhanh LLDB](https://lldb.llvm.org/use/tutorial.html)
- [Hướng dẫn Debug Swift của Ray Wenderlich](https://www.raywenderlich.com/966538-arc-and-memory-management-in-swift)
