---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:05.940464-07:00
description: "S\u1EED d\u1EE5ng debugger c\xF3 ngh\u0129a l\xE0 s\u1EED d\u1EE5ng\
  \ c\xE1c c\xF4ng c\u1EE5 chuy\xEAn bi\u1EC7t \u0111\u1EC3 ki\u1EC3m tra v\xE0 ki\u1EC3\
  m so\xE1t m\xE3 c\u1EE7a b\u1EA1n khi n\xF3 ch\u1EA1y. \u0110i\u1EC1u n\xE0y th\u1EF1\
  c s\u1EF1 quan tr\u1ECDng b\u1EDFi v\xEC n\xF3 cho\u2026"
lastmod: '2024-02-25T18:49:35.449689-07:00'
model: gpt-4-0125-preview
summary: "S\u1EED d\u1EE5ng debugger c\xF3 ngh\u0129a l\xE0 s\u1EED d\u1EE5ng c\xE1\
  c c\xF4ng c\u1EE5 chuy\xEAn bi\u1EC7t \u0111\u1EC3 ki\u1EC3m tra v\xE0 ki\u1EC3\
  m so\xE1t m\xE3 c\u1EE7a b\u1EA1n khi n\xF3 ch\u1EA1y. \u0110i\u1EC1u n\xE0y th\u1EF1\
  c s\u1EF1 quan tr\u1ECDng b\u1EDFi v\xEC n\xF3 cho\u2026"
title: "S\u1EED d\u1EE5ng b\u1ED9 g\u1EE1 l\u1ED7i"
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
- [Hướng dẫn Debug Xcode của Apple](https://developer.apple.com/documentation/xcode/debugging/)
- [Hướng dẫn Nhanh LLDB](https://lldb.llvm.org/use/tutorial.html)
- [Hướng dẫn Debug Swift của Ray Wenderlich](https://www.raywenderlich.com/966538-arc-and-memory-management-in-swift)
