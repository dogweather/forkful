---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:11.288769-07:00
description: "L\xE0m Th\u1EBF N\xE0o: Trong Swift, b\u1EA1n c\xF3 th\u1EC3 vi\u1EBF\
  t log ra b\u1EA3ng \u0111i\u1EC1u khi\u1EC3n v\u1EDBi c\xE1c c\xE2u l\u1EC7nh in\
  \ ho\u1EB7c API `os.log` linh ho\u1EA1t h\u01A1n, n\xF3 k\u1EBFt n\u1ED1i v\xE0\
  o H\u1EC7 th\u1ED1ng Ghi log\u2026"
lastmod: '2024-03-13T22:44:37.106404-06:00'
model: gpt-4-0125-preview
summary: "Trong Swift, b\u1EA1n c\xF3 th\u1EC3 vi\u1EBFt log ra b\u1EA3ng \u0111i\u1EC1\
  u khi\u1EC3n v\u1EDBi c\xE1c c\xE2u l\u1EC7nh in ho\u1EB7c API `os.log` linh ho\u1EA1\
  t h\u01A1n, n\xF3 k\u1EBFt n\u1ED1i v\xE0o H\u1EC7 th\u1ED1ng Ghi log Th\u1ED1ng\
  \ nh\u1EA5t tr\xEAn c\xE1c n\u1EC1n t\u1EA3ng c\u1EE7a Apple."
title: Ghi log
weight: 17
---

## Làm Thế Nào:
Trong Swift, bạn có thể viết log ra bảng điều khiển với các câu lệnh in hoặc API `os.log` linh hoạt hơn, nó kết nối vào Hệ thống Ghi log Thống nhất trên các nền tảng của Apple.

```Swift
import os.log

let logger = OSLog(subsystem: "com.yourapp.domain", category: "network")

func fetchData() {
    // Câu lệnh in đơn giản
    print("Bắt đầu lấy dữ liệu")
    
    // Ghi nhật ký sự kiện cấp độ thông tin bằng os.log
    os_log(.info, log: logger, "Đang lấy dữ liệu từ API.")
    
    do {
        let data = try performNetworkRequest()
        // Ghi nhật ký sự kiện cấp độ gỡ lỗi
        os_log(.debug, log: logger, "Dữ liệu nhận được: %@", data.description)
    } catch {
        // Ghi nhật ký sự kiện cấp độ lỗi
        os_log(.error, log: logger, "Lấy dữ liệu thất bại: %@", error.localizedDescription)
    }
}

func performNetworkRequest() throws -> Data {
    // Mô phỏng một yêu cầu mạng
    return Data()
}
```

Đầu ra mẫu trên bảng điều khiển có thể như sau:

```
Bắt đầu lấy dữ liệu
Đang lấy dữ liệu từ API.
Dữ liệu nhận được: Một số bytes dữ liệu...
```

Đối với lỗi, có thể là:

```
Lấy dữ liệu thất bại: Kết nối internet dường như đang ngoại tuyến.
```

## Tìm Hiểu Sâu
Ghi log trong Swift trở nên mạnh mẽ và hiệu quả hơn với Hệ thống Ghi log Thống nhất được giới thiệu trong iOS 10 và macOS Sierra. Khác với câu lệnh `print` đi thẳng vào bảng điều khiển, hệ thống này dựa trên hoạt động, và cho phép bạn lọc thông điệp log dựa trên mức độ quan trọng và dù chúng là bản build dành cho gỡ lỗi hay phát hành.

Bối cảnh lịch sử đặt ra sự tiến hóa của việc ghi log trong iOS và macOS từ các câu lệnh in cơ bản đến các công cụ toàn diện tích hợp với ứng dụng Instruments và Console, cung cấp cách phân tích log tinh vi.

Có một loạt các lựa chọn thay thế cho việc ghi log trong Swift, như các thư viện bên thứ ba như CocoaLumberjack, nó cung cấp một lớp macro trên Hệ thống Ghi log Thống nhất. Nó cung cấp kiểm soát nâng cao về định dạng log, quản lý tệp và các tùy chọn hiệu suất.

Cuối cùng, chi tiết thực hiện; OSLog được thiết kế không chỉ hiệu quả mà còn ý thức về bảo mật, với khả năng làm mờ dữ liệu riêng tư khi ghi log. Nó phân loại các log vào cấp độ lỗi, thông tin và gỡ lỗi, mỗi cấp độ cung cấp một độ chi tiết khác nhau cho việc gỡ lỗi.

## Tham Khảo
- [Tài liệu Ghi log Thống nhất của Apple](https://developer.apple.com/documentation/os/logging)
- [Hướng dẫn Ghi log trong Swift của Ray Wenderlich](https://www.raywenderlich.com/605079-logging-in-swift-oslog)
- [Kho lưu trữ GitHub CocoaLumberjack](https://github.com/CocoaLumberjack/CocoaLumberjack)
