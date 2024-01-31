---
title:                "Tải trang web"
date:                  2024-01-28T21:59:40.674565-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tải trang web"

category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/python/downloading-a-web-page.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Tải một trang web về cơ bản là việc lấy dữ liệu từ URL bạn chỉ định và kéo nó về máy tính của bạn. Lập trình viên làm điều này để phân tích dữ liệu, theo dõi sự thay đổi, hoặc tự động hóa tương tác với các trang web.

## Làm thế nào:

Chúng ta sẽ sử dụng thư viện `requests` của Python. Nếu bạn chưa có nó, hãy cài đặt với `pip install requests`. Dưới đây là một ví dụ nhanh:

```python
import requests

url = 'https://www.example.com'
response = requests.get(url)

if response.ok:
    html_content = response.text
    print(html_content)
else:
    print("Không thể truy xuất trang web")

```

Khi kịch bản này được chạy, nếu thành công, bạn sẽ thấy nội dung HTML của "https://www.example.com" được in ra trong bảng điều khiển của bạn.

## Đi sâu hơn

Trước `requests`, Python đã có `urllib`. Nó vẫn còn đó, nhưng `requests` đã chiếm lĩnh sân khấu với giao diện thân thiện, dễ sử dụng hơn. `requests` được phát hành vào năm 2011 bởi Kenneth Reitz và đã trở thành tiêu chuẩn vàng cho HTTP trong Python từ đó đến nay. Nhưng không chỉ là đơn giản – `requests` còn mạnh mẽ, cung cấp các tính năng như đối tượng phiên, bảo quản cookie, và xử lý tự động các chứng chỉ SSL.

Có những phương thức thay thế như `http.client`, là thấp hơn so với `requests`, và thư viện bên ngoài như `aiohttp` cho các hoạt động bất đồng bộ. Sâu dưới cơ bản, bất kể sự lựa chọn của bạn, những thư viện này tương tác với các máy chủ web, gửi yêu cầu HTTP, và xử lý phản hồi.

Khi tải trang, điều quan trọng là phải xem xét các quy tắc của đường: tôn trọng các tệp `robots.txt` để biết bạn được phép ở đâu, và không làm quá tải máy chủ – hãy làm chậm yêu cầu của bạn. Ngoài ra, hãy nhớ rằng các trang web có thể kéo nội dung động với JavaScript mà sẽ không được bắt với một yêu cầu HTTP đơn giản.

## Xem thêm:

- Tài liệu `requests`: https://requests.readthedocs.io/en/master/
- Thông tin `urllib`: https://docs.python.org/3/library/urllib.html
- Giới thiệu `robots.txt`: https://www.robotstxt.org
- `aiohttp` cho yêu cầu web bất đồng bộ: https://docs.aiohttp.org/en/stable/
