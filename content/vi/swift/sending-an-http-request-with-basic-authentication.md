---
title:                "Gửi một yêu cầu HTTP với xác thực cơ bản"
date:                  2024-01-28T22:08:34.902502-07:00
model:                 gpt-4-0125-preview
simple_title:         "Gửi một yêu cầu HTTP với xác thực cơ bản"

category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/swift/sending-an-http-request-with-basic-authentication.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại Sao?

Gửi một yêu cầu HTTP với xác thực cơ bản bao gồm việc đính kèm tên người dùng và mật khẩu vào một yêu cầu để truy cập vào nội dung web được bảo vệ. Lập trình viên thực hiện điều này để truy cập vào API hoặc các tài nguyên bị hạn chế chỉ dành cho người dùng được ủy quyền.

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
