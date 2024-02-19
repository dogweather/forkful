---
aliases:
- /vi/swift/logging/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:11.288769-07:00
description: "Ghi log l\xE0 qu\xE1 tr\xECnh ghi l\u1EA1i h\xE0nh vi, l\u1ED7i v\xE0\
  \ c\xE1c th\xF4ng tin quan tr\u1ECDng kh\xE1c c\u1EE7a \u1EE9ng d\u1EE5ng v\xE0\
  o m\u1ED9t ph\u01B0\u01A1ng ti\u1EC7n l\u01B0u tr\u1EEF l\xE2u d\xE0i, nh\u01B0\
  \ t\u1EC7p tin ho\u1EB7c c\u01A1 s\u1EDF d\u1EEF\u2026"
lastmod: 2024-02-18 23:08:51.097295
model: gpt-4-0125-preview
summary: "Ghi log l\xE0 qu\xE1 tr\xECnh ghi l\u1EA1i h\xE0nh vi, l\u1ED7i v\xE0 c\xE1\
  c th\xF4ng tin quan tr\u1ECDng kh\xE1c c\u1EE7a \u1EE9ng d\u1EE5ng v\xE0o m\u1ED9\
  t ph\u01B0\u01A1ng ti\u1EC7n l\u01B0u tr\u1EEF l\xE2u d\xE0i, nh\u01B0 t\u1EC7p\
  \ tin ho\u1EB7c c\u01A1 s\u1EDF d\u1EEF\u2026"
title: Ghi log
---

{{< edit_this_page >}}

## Gì & Tại Sao?
Ghi log là quá trình ghi lại hành vi, lỗi và các thông tin quan trọng khác của ứng dụng vào một phương tiện lưu trữ lâu dài, như tệp tin hoặc cơ sở dữ liệu. Lập trình viên thực hiện điều này để theo dõi tình hình và hiệu suất của ứng dụng, để gỡ lỗi và để giữ mắt trên những gì đang diễn ra bên dưới hạ tầng trong môi trường sản xuất.

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
