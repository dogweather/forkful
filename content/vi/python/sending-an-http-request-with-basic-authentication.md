---
title:                "Gửi một yêu cầu HTTP với xác thực cơ bản"
date:                  2024-01-28T22:08:29.783459-07:00
model:                 gpt-4-0125-preview
simple_title:         "Gửi một yêu cầu HTTP với xác thực cơ bản"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/python/sending-an-http-request-with-basic-authentication.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại Sao?

Việc gửi một yêu cầu HTTP với xác thực cơ bản bao gồm việc đưa tên người dùng và mật khẩu vào một yêu cầu gửi đến máy chủ để chứng minh rằng bạn được phép truy cập. Lập trình viên làm điều này để tương tác với các API hoặc dịch vụ web được khóa sau cổng thông tin đăng nhập của người dùng.

## Cách thực hiện:

Dưới đây là cách bạn yêu cầu Python nói chuyện với máy chủ sử dụng Basic Auth.

```Python
import requests
from requests.auth import HTTPBasicAuth

# Thay thế bằng thông tin đăng nhập thực tế và điểm cuối API mà bạn đang truy cập
username = 'cooluser'
password = 'supersecretpassword'
url = 'https://api.someservice.com/data'

phản_hồi = requests.get(url, auth=HTTPBasicAuth(username, password))

# Kiểm tra xem chúng ta nhận lại được gì
print(phản_hồi.status_code)
print(phản_hồi.json())  # giả sử phản hồi là ở định dạng JSON
```

Đầu ra có thể trông như thế này nếu mọi thứ suôn sẻ:

```
200
{'data': 'Thứ bí mật của bạn!'}
```

Nhưng nếu bạn nhập sai thông tin đăng nhập:

```
401
```

Đó là dấu hiệu không được vào đó.

## Sâu hơn

Lịch sử, HTTP Basic Auth là cách cổ điển nhất của bảo mật web, một cách đơn giản để thực hiện bắt tay bí mật với một trang web. Nó không an toàn lắm nếu tự nó vì nó gửi thông tin đăng nhập dưới dạng văn bản rõ, chỉ được mã hóa base64 – không được mã hóa. Hãy luôn sử dụng HTTPS để ngăn thông tin đăng nhập dễ bị bắt giữ như lấy kẹo từ trẻ con.

Có những phương pháp thay thế an toàn hơn, như Xác thực Truy cập Tiêu hóa nơi mật khẩu không bao giờ được gửi rõ ràng qua mạng. OAuth là một cái lớn khác, đặc biệt cho các API ngày nay. Nó giống như cấp một tấm vé VIP tạm thời hơn là hiển thị ID mỗi khi.

Phía sau hậu trường, thư viện `requests` đang mã hóa tên người dùng và mật khẩu của bạn và gắn chúng vào một tiêu đề `Authorization` được định dạng như `Basic base64encodedcredentials`. Máy chủ giải mã tiêu đề này, kiểm tra thông tin đăng nhập của bạn, và nếu bạn hợp lệ, cho phép bạn truy cập.

## Xem Thêm

- Tài liệu chính thức của thư viện `requests` cung cấp thông tin chi tiết về xác thực và nhiều hơn nữa: https://docs.python-requests.org/en/latest/
- `http.client` cho những người muốn làm việc mà không cần thư viện bên thứ ba: https://docs.python.org/3/library/http.client.html
- Real Python đào sâu vào cơ bản của HTTP và Python: https://realpython.com/python-requests/
