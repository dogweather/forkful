---
title:                "Ghi vào lỗi chuẩn"
date:                  2024-01-28T22:13:59.690996-07:00
model:                 gpt-4-0125-preview
simple_title:         "Ghi vào lỗi chuẩn"

category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/swift/writing-to-standard-error.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại Sao?
Việc ghi vào kênh lỗi chuẩn (`stderr`) chuyển các thông điệp quan trọng đến một luồng xuất ra đặc biệt được dành riêng cho lỗi, tách biệt khỏi luồng xuất chính (`stdout`). Các lập trình viên sử dụng nó để ghi lại lỗi và thông điệp chẩn đoán để chúng không lẫn vào dữ liệu chương trình bình thường và có thể dễ dàng được theo dõi hoặc chuyển hướng.

## Làm thế nào:
Swift giúp việc ghi vào `stderr` trở nên đơn giản. Xem ví dụ dưới đây:

```Swift
import Foundation

// Ghi vào lỗi chuẩn
func writeToStdErr(_ message: String) {
    if let data = "\(message)\n".data(using: .utf8) {
        FileHandle.standardError.write(data)
    }
}

// Ví dụ sử dụng
writeToStdErr("Rất tiếc! Đã xảy ra lỗi.")

// Khi chạy trong console đầu ra có thể trông như thế này
// (mặc dù điều này không thấy được trong console của Xcode):
// Rất tiếc! Đã xảy ra lỗi.
```

## Sâu Hơn
Trong những ngày lập trình trước, việc phân biệt giữa `stdout` (luồng xuất chuẩn) và `stderr` (lỗi chuẩn) là rất quan trọng để phân tích đầu ra lệnh và xử lý lỗi. Các ngôn ngữ khác cung cấp các cấu trúc tương tự, và trong các hệ thống dựa trên Unix, những luồng này trực tiếp liên quan đến terminal. Việc thực hiện điều này trong Swift kết nối với cùng một nguyên tắc cơ bản, nơi `stderr` phục vụ như một luồng không đệm, có nghĩa là nó ngay lập tức xóa đầu ra. Hành vi này rất quan trọng cho việc báo cáo lỗi theo thời gian thực.

Các phương án thay thế bao gồm các khung ghi nhật ký có thể cung cấp thêm tính năng, như mức độ nhật ký và định dạng tin nhắn. Thư viện tiêu chuẩn của riêng Swift khá đơn giản; nếu bạn cần sự tinh vi, có lẽ bạn sẽ nhìn vào các thư viện bên thứ ba hoặc hệ thống ghi nhật ký thống nhất của Apple.

## Xem Thêm
Để hiểu sâu hơn và có thêm bối cảnh, hãy xem những nguồn lực này:

- [Tài liệu Ghi nhật ký Thống nhất của Apple](https://developer.apple.com/documentation/os/logging)
- [Tham khảo Thư viện Chuẩn của Swift cho FileHandle](https://developer.apple.com/documentation/foundation/filehandle)
