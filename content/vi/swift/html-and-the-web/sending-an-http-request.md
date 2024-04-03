---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:30.989014-07:00
description: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP gi\u1ED1ng nh\u01B0 g\xF5 c\u1EED\
  a m\u1ED9t m\xE1y ch\u1EE7 web, y\xEAu c\u1EA7u d\u1EEF li\u1EC7u ho\u1EB7c ph\u1EE5\
  c v\u1EE5 m\u1ED9t s\u1ED1 d\u1EEF li\u1EC7u. L\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111\
  i\u1EC1u n\xE0y \u0111\u1EC3 t\u01B0\u01A1ng t\xE1c v\u1EDBi c\xE1c API,\u2026"
lastmod: '2024-03-13T22:44:37.092522-06:00'
model: gpt-4-0125-preview
summary: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP gi\u1ED1ng nh\u01B0 g\xF5 c\u1EED\
  a m\u1ED9t m\xE1y ch\u1EE7 web, y\xEAu c\u1EA7u d\u1EEF li\u1EC7u ho\u1EB7c ph\u1EE5\
  c v\u1EE5 m\u1ED9t s\u1ED1 d\u1EEF li\u1EC7u."
title: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP"
weight: 44
---

## Cách thực hiện:
Swift giúp việc gửi yêu cầu HTTP trở nên dễ dàng thông qua lớp `URLSession`. Dưới đây là một ví dụ đơn giản về yêu cầu GET:

```Swift
import Foundation

// URL của tài nguyên bạn đang yêu cầu
if let url = URL(string: "https://api.example.com/data") {

    // Tạo một URLSessionDataTask
    let task = URLSession.shared.dataTask(with: url) { data, response, error in
        
        // Kiểm tra nếu có lỗi
        if let error = error {
            print("Lỗi khi tải dữ liệu: \(error)")
            return
        }
        
        // Kiểm tra nếu nhận được phản hồi và dữ liệu hợp lệ
        if let httpResponse = response as? HTTPURLResponse, 
           httpResponse.statusCode == 200,
           let data = data {
            
            // Chuyển đổi dữ liệu thành chuỗi và in ra
            let dataString = String(decoding: data, as: UTF8.self)
            print(dataString)
        }
    }
    // Bắt đầu công việc
    task.resume()
}

// Đầu ra mẫu sẽ là nội dung được tải từ API.
```

Để gửi một yêu cầu POST với JSON:

```Swift
import Foundation
import CoreFoundation

// Điểm kết thúc API của bạn
if let url = URL(string: "https://api.example.com/submit") {

    // Chuẩn bị dữ liệu bạn muốn gửi
    let dictionary = ["key": "value"]
    guard let jsonData = try? JSONSerialization.data(withJSONObject: dictionary) else {
        print("Lỗi: Không thể tạo JSON từ dictionary")
        return
    }
    
    // Chuẩn bị URLRequest
    var request = URLRequest(url: url)
    request.httpMethod = "POST"
    request.setValue("application/json", forHTTPHeaderField: "Content-Type")
    request.httpBody = jsonData
    
    // Tạo và bắt đầu công việc
    let task = URLSession.shared.dataTask(with: request) { data, response, error in
        // Xử lý phản hồi ở đây
    }
    task.resume()
}

// Đầu ra phụ thuộc vào phản hồi từ máy chủ. Không có đầu ra tiêu chuẩn.
```

## Tìm hiểu sâu:
Yêu cầu HTTP là cơ sở của giao tiếp web. Chúng đã tồn tại từ những ngày đầu của web, cho phép có một cách tiêu chuẩn để trao đổi dữ liệu.

Các lựa chọn thay thế cho `URLSession` bao gồm các thư viện bên thứ ba như Alamofire giúp đơn giản hóa cú pháp và bổ sung thêm chức năng. Tuy nhiên, `URLSession` vẫn là lựa chọn bản địa để thực hiện các cuộc gọi mạng và Apple tiếp tục cập nhật nó với các tính năng mạng mới nhất và các tiêu chuẩn bảo mật.

Một chi tiết cài đặt cần lưu ý là các yêu cầu mạng một cách tự nhiên là bất đồng bộ trong Swift. Chúng chạy ở chế độ nền, cho phép ứng dụng tiếp tục phản hồi. Khi có phản hồi trả về, một bộ xử lý hoàn thành được gọi. Rất quan trọng để xử lý quản lý luồng đúng cách, đặc biệt là khi cập nhật giao diện người dùng, điều này phải xảy ra trên luồng chính.

## Xem thêm:
- [URLSession | Tài liệu cho Nhà phát triển Apple](https://developer.apple.com/documentation/foundation/urlsession)
- [Làm việc với JSON trong Swift](https://developer.apple.com/swift/blog/?id=37)
- [Kho lưu trữ GitHub của Alamofire](https://github.com/Alamofire/Alamofire)
