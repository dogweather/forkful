---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:49.950044-07:00
description: "T\u1EA3i m\u1ED9t trang web xu\u1ED1ng c\xF3 ngh\u0129a l\xE0 l\u1EA5\
  y d\u1EEF li\u1EC7u t\u1EEB web v\xE0 \u0111\u01B0a n\xF3 v\xE0o \u1EE9ng d\u1EE5\
  ng c\u1EE7a b\u1EA1n. L\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n vi\u1EC7c n\xE0\
  y \u0111\u1EC3 t\xECm n\u1ED9i dung, t\u01B0\u01A1ng t\xE1c v\u1EDBi\u2026"
lastmod: 2024-02-19 22:04:56.302043
model: gpt-4-0125-preview
summary: "T\u1EA3i m\u1ED9t trang web xu\u1ED1ng c\xF3 ngh\u0129a l\xE0 l\u1EA5y d\u1EEF\
  \ li\u1EC7u t\u1EEB web v\xE0 \u0111\u01B0a n\xF3 v\xE0o \u1EE9ng d\u1EE5ng c\u1EE7\
  a b\u1EA1n. L\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n vi\u1EC7c n\xE0y \u0111\
  \u1EC3 t\xECm n\u1ED9i dung, t\u01B0\u01A1ng t\xE1c v\u1EDBi\u2026"
title: "T\u1EA3i trang web"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Tải một trang web xuống có nghĩa là lấy dữ liệu từ web và đưa nó vào ứng dụng của bạn. Lập trình viên thực hiện việc này để tìm nội dung, tương tác với dịch vụ trực tuyến hoặc thu thập dữ liệu.

## Làm thế nào:
Hãy sử dụng `URLSession` để thực hiện công việc này. Swift làm cho nó trở nên đơn giản và dễ dàng.

```Swift
import Foundation

let url = URL(string: "https://www.example.com")!
let task = URLSession.shared.dataTask(with: url) { data, response, error in
    nếu let error = error {
        print("Lỗi:", error)
        trở lại
    }

    if let httpResponse = response as? HTTPURLResponse, (200...299).contains(httpResponse.statusCode) {
        if let mimeType = httpResponse.mimeType, mimeType == "text/html",
           let data = data, let string = String(data: data, encoding: .utf8) {
            print("Nội dung trang web đã tải xuống:")
            print(string)
        } else {
            print("Loại MIME hoặc mã hóa không hợp lệ.")
        }
    } else {
        print("Máy chủ phản hồi với lỗi.")
    }
}
task.resume()
// Đảm bảo chạy chương trình cho đến khi công việc hoàn thành
RunLoop.current.run()
```

Đầu ra mẫu có thể trông như thế này:

```
Nội dung trang web đã tải xuống:
<!doctype html>...
```

## Đào sâu
API `URLSession` đã có từ iOS 7 và macOS 10.9. Nó đã tạo nên một bước ngoặt lớn khi đó, thay thế cho `NSURLConnection` cũ và cồng kềnh hơn. Mặc dù `URLSession` mạnh mẽ và linh hoạt, bạn cũng có thể xem xét các thư viện bên thứ ba như Alamofire cho nhu cầu mạng phức tạp hơn. 

Khi thực hiện, hãy nhớ rằng các yêu cầu mạng là bất đồng bộ. Điều này có nghĩa là ứng dụng của bạn có thể tiếp tục với những tác vụ khác trong khi chờ máy chủ trả lời. Thêm nữa, sử dụng `URLSession` một cách đúng đắn bao gồm xử lý lỗi một cách mềm dẻo và kiểm tra trạng thái phản hồi từ máy chủ. Kiểm tra kiểu MIME rất quan trọng để đảm bảo bạn đang nhận HTML, không phải các loại tệp khác như JSON hay hình ảnh.

## Xem thêm
Đào sâu hơn hoặc khám phá các lựa chọn khác:
- Tài liệu `URLSession` của Apple: [URLSession](https://developer.apple.com/documentation/foundation/urlsession)
- Mạng lưới Swift với Alamofire: [Alamofire](https://github.com/Alamofire/Alamofire)
- Mẫu async/await của `URLSession` trong iOS 15+: [URLSession async/await](https://developer.apple.com/videos/play/wwdc2021/10054/)
