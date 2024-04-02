---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:59.690996-07:00
description: "Vi\u1EC7c ghi v\xE0o k\xEAnh l\u1ED7i chu\u1EA9n (`stderr`) chuy\u1EC3\
  n c\xE1c th\xF4ng \u0111i\u1EC7p quan tr\u1ECDng \u0111\u1EBFn m\u1ED9t lu\u1ED3\
  ng xu\u1EA5t ra \u0111\u1EB7c bi\u1EC7t \u0111\u01B0\u1EE3c d\xE0nh ri\xEAng cho\
  \ l\u1ED7i, t\xE1ch bi\u1EC7t kh\u1ECFi lu\u1ED3ng xu\u1EA5t\u2026"
lastmod: '2024-03-13T22:44:37.119272-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c ghi v\xE0o k\xEAnh l\u1ED7i chu\u1EA9n (`stderr`) chuy\u1EC3n\
  \ c\xE1c th\xF4ng \u0111i\u1EC7p quan tr\u1ECDng \u0111\u1EBFn m\u1ED9t lu\u1ED3\
  ng xu\u1EA5t ra \u0111\u1EB7c bi\u1EC7t \u0111\u01B0\u1EE3c d\xE0nh ri\xEAng cho\
  \ l\u1ED7i, t\xE1ch bi\u1EC7t kh\u1ECFi lu\u1ED3ng xu\u1EA5t\u2026"
title: "Ghi v\xE0o l\u1ED7i chu\u1EA9n"
weight: 25
---

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
