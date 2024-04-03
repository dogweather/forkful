---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:29.783459-07:00
description: "Vi\u1EC7c g\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP v\u1EDBi x\xE1c th\u1EF1\
  c c\u01A1 b\u1EA3n bao g\u1ED3m vi\u1EC7c \u0111\u01B0a t\xEAn ng\u01B0\u1EDDi d\xF9\
  ng v\xE0 m\u1EADt kh\u1EA9u v\xE0o m\u1ED9t y\xEAu c\u1EA7u g\u1EEDi \u0111\u1EBF\
  n m\xE1y ch\u1EE7 \u0111\u1EC3 ch\u1EE9ng minh r\u1EB1ng b\u1EA1n \u0111\u01B0\u1EE3\
  c ph\xE9p\u2026"
lastmod: '2024-03-13T22:44:36.096455-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c g\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP v\u1EDBi x\xE1c th\u1EF1\
  c c\u01A1 b\u1EA3n bao g\u1ED3m vi\u1EC7c \u0111\u01B0a t\xEAn ng\u01B0\u1EDDi d\xF9\
  ng v\xE0 m\u1EADt kh\u1EA9u v\xE0o m\u1ED9t y\xEAu c\u1EA7u g\u1EEDi \u0111\u1EBF\
  n m\xE1y ch\u1EE7 \u0111\u1EC3 ch\u1EE9ng minh r\u1EB1ng b\u1EA1n \u0111\u01B0\u1EE3\
  c ph\xE9p truy c\u1EADp."
title: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP v\u1EDBi x\xE1c th\u1EF1c c\u01A1 b\u1EA3\
  n"
weight: 45
---

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
