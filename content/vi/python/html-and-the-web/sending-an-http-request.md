---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:03.100853-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Th\u01B0 vi\u1EC7n `requests` b\xEAn th\u1EE9\
  \ ba c\u1EE7a Python l\xE0m vi\u1EC7c g\u1ECDi HTTP tr\u1EDF n\xEAn d\u1EC5 d\xE0\
  ng. D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch g\u1EEDi m\u1ED9t y\xEAu c\u1EA7u GET\
  \ \u0111\u01A1n gi\u1EA3n."
lastmod: '2024-03-13T22:44:36.092519-06:00'
model: gpt-4-0125-preview
summary: "Th\u01B0 vi\u1EC7n `requests` b\xEAn th\u1EE9 ba c\u1EE7a Python l\xE0m\
  \ vi\u1EC7c g\u1ECDi HTTP tr\u1EDF n\xEAn d\u1EC5 d\xE0ng."
title: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP"
weight: 44
---

## Cách thực hiện:
Thư viện `requests` bên thứ ba của Python làm việc gọi HTTP trở nên dễ dàng. Dưới đây là cách gửi một yêu cầu GET đơn giản:

```python
import requests

response = requests.get('https://api.example.com/data')
print(response.status_code)  # Xuất mã trạng thái của phản hồi
print(response.json())      # Nếu phản hồi mang theo JSON, in nó dưới dạng dict Python
```

Yêu cầu POST chi tiết hơn với payload JSON và tiêu đề tùy chỉnh:

```python
import requests
import json

url = "https://api.example.com/submit"
data = {'key': 'value'}
headers = {'Content-Type': 'application/json'}

response = requests.post(url, data=json.dumps(data), headers=headers)

print(response.status_code)
print(response.json())
```

## Sâu hơn
Yêu cầu HTTP là cách mạng web hoạt động — chúng đã tồn tại kể từ đầu những năm 90. Các lựa chọn thay thế cho `requests` của Python bao gồm thư viện chuẩn `urllib`, nhưng nó hơi khó sử dụng hơn.

Hiểu cách gửi yêu cầu HTTP bao gồm biết về các phương thức (GET, POST, PUT, DELETE, v.v.), mã trạng thái (ví dụ, 200 OK, 404 Not Found), tiêu đề, và dữ liệu thân.

Đối với các yêu cầu luồng hoặc không đồng bộ, bạn có thể muốn khám phá phiên bản không đồng bộ của `requests` hoặc gói `aiohttp`. Bên dưới, những thư viện này sử dụng `socket` của Python cho giao tiếp mạng thô.

Lịch sử, `requests` được coi là lựa chọn hàng đầu do sự đơn giản và mạnh mẽ của nó, nhưng `httpx`, một thư viện tương thích không đồng bộ mới hơn, đang dần trở nên phổ biến.

## Xem thêm
- Tài liệu thư viện `requests`: https://requests.readthedocs.io
- Giải thích mã trạng thái HTTP: https://developer.mozilla.org/vi/docs/Web/HTTP/Status
- Tài liệu `urllib` của Python: https://docs.python.org/3/library/urllib.html
- Thư viện `httpx` cho yêu cầu HTTP không đồng bộ: https://www.python-httpx.org
