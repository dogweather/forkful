---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:34.902502-07:00
description: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP v\u1EDBi x\xE1c th\u1EF1c c\u01A1\
  \ b\u1EA3n bao g\u1ED3m vi\u1EC7c \u0111\xEDnh k\xE8m t\xEAn ng\u01B0\u1EDDi d\xF9\
  ng v\xE0 m\u1EADt kh\u1EA9u v\xE0o m\u1ED9t y\xEAu c\u1EA7u \u0111\u1EC3 truy c\u1EAD\
  p v\xE0o n\u1ED9i dung web \u0111\u01B0\u1EE3c b\u1EA3o v\u1EC7. L\u1EADp\u2026"
lastmod: '2024-03-13T22:44:37.096763-06:00'
model: gpt-4-0125-preview
summary: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP v\u1EDBi x\xE1c th\u1EF1c c\u01A1\
  \ b\u1EA3n bao g\u1ED3m vi\u1EC7c \u0111\xEDnh k\xE8m t\xEAn ng\u01B0\u1EDDi d\xF9\
  ng v\xE0 m\u1EADt kh\u1EA9u v\xE0o m\u1ED9t y\xEAu c\u1EA7u \u0111\u1EC3 truy c\u1EAD\
  p v\xE0o n\u1ED9i dung web \u0111\u01B0\u1EE3c b\u1EA3o v\u1EC7."
title: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP v\u1EDBi x\xE1c th\u1EF1c c\u01A1 b\u1EA3\
  n"
weight: 45
---

## Cách thực hiện:
Dưới đây là cách gửi một yêu cầu HTTP với xác thực cơ bản trong Swift:

```Swift
import Foundation

// Điểm cuối API của bạn
let url = URL(string: "https://example.com/api/data")!

// Các thông tin đăng nhập của bạn
let username = "user"
let password = "password"

// Tạo dữ liệu đăng nhập và chuyển đổi thành chuỗi base64
let loginData = String(format: "%@:%@", username, password).data(using: String.Encoding.utf8)!
let base64LoginData = loginData.base64EncodedString()

// Tạo yêu cầu
var request = URLRequest(url: url)
request.httpMethod = "GET"
request.setValue("Basic \(base64LoginData)", forHTTPHeaderField: "Authorization")

// Gửi yêu cầu
let session = URLSession.shared
let dataTask = session.dataTask(with: request) { data, response, error in
    if let error = error {
        print("Lỗi: \(error)") // Xử lý lỗi
    } else if let data = data, let string = String(data: data, encoding: .utf8) {
        print("Phản hồi: \(string)") // Xử lý phản hồi
    }
}

dataTask.resume()
```

Kết quả sẽ là dữ liệu được trả về từ API, hoặc một thông báo lỗi nếu có gì đó không đúng.

## Sâu hơn nữa
Trở lại những ngày đầu của web, xác thực cơ bản là một cách nhanh chóng để bảo vệ tài nguyên. Độ đơn giản của nó đã làm cho nó được áp dụng rộng rãi mặc dù kém an toàn hơn so với các phương thức hiện đại như OAuth vì thông tin xác thực không được mã hóa, chỉ được mã hóa.

Bên cạnh xác thực cơ bản, các lựa chọn khác bao gồm xác thực digest, khóa API, OAuth, hoặc JWT (JSON Web Tokens). Mỗi lựa chọn đều có những ưu và nhược điểm về mặt bảo mật, dễ sử dụng và mức độ bảo vệ được cung cấp.

Khi gửi một yêu cầu HTTP với xác thực cơ bản, điều tốt nhất là đảm bảo bạn đang sử dụng HTTPS, để thông tin xác thực được mã hóa của bạn được truyền đi an toàn. Ngoài ra, hãy tránh mã hóa thông tin xác thực trực tiếp trong mã; thay vào đó, hãy sử dụng biến môi trường hoặc két an toàn.

## Xem thêm
- [URLSession của Apple](https://developer.apple.com/documentation/foundation/urlsession)
- [RFC Xác Thực HTTP Cơ Bản](https://tools.ietf.org/html/rfc7617)
- [OAuth 2.0](https://oauth.net/2/)
