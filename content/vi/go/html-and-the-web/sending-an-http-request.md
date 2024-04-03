---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:09:15.936698-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong Go, vi\u1EC7c g\u1EEDi m\u1ED9t y\xEA\
  u c\u1EA7u HTTP v\xE0 x\u1EED l\xFD ph\u1EA3n h\u1ED3i li\xEAn quan \u0111\u1EBF\
  n vi\u1EC7c s\u1EED d\u1EE5ng g\xF3i `net/http`. D\u01B0\u1EDBi \u0111\xE2y l\xE0\
  \ m\u1ED9t v\xED d\u1EE5 t\u1EEBng b\u01B0\u1EDBc cho th\u1EA5y\u2026"
lastmod: '2024-03-13T22:44:35.977773-06:00'
model: gpt-4-0125-preview
summary: "Trong Go, vi\u1EC7c g\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP v\xE0 x\u1EED\
  \ l\xFD ph\u1EA3n h\u1ED3i li\xEAn quan \u0111\u1EBFn vi\u1EC7c s\u1EED d\u1EE5\
  ng g\xF3i `net/http`."
title: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP"
weight: 44
---

## Làm thế nào:
Trong Go, việc gửi một yêu cầu HTTP và xử lý phản hồi liên quan đến việc sử dụng gói `net/http`. Dưới đây là một ví dụ từng bước cho thấy cách gửi một yêu cầu GET đơn giản và đọc phản hồi:

```go
package main

import (
    "fmt"
    "io/ioutil"
    "log"
    "net/http"
)

func main() {
    // Định nghĩa URL của tài nguyên
    url := "http://example.com"

    // Sử dụng http.Get để gửi yêu cầu GET
    resp, err := http.Get(url)
    if err != nil {
        log.Fatal(err)
    }
    // Đóng body của phản hồi khi hàm kết thúc
    defer resp.Body.Close()

    // Đọc body của phản hồi
    body, err := ioutil.ReadAll(resp.Body)
    if err != nil {
        log.Fatal(err)
    }

    // Chuyển body phản hồi thành chuỗi và in ra
    fmt.Println(string(body))
}
```

Mẫu đầu ra (được rút ngắn cho gọn):
```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```

Để gửi một yêu cầu POST với dữ liệu biểu mẫu, bạn có thể sử dụng `http.PostForm`:

```go
package main

import (
    "fmt"
    "io/ioutil"
    "net/http"
    "net/url"
)

func main() {
    // Định nghĩa URL và dữ liệu biểu mẫu
    url := "http://example.com/form"
    data := url.Values{}
    data.Set("key", "value")

    // Gửi yêu cầu POST với dữ liệu biểu mẫu
    resp, err := http.PostForm(url, data)
    if err != nil {
        panic(err)
    }
    defer resp.Body.Close()

    // Đọc và in phản hồi
    body, err := ioutil.ReadAll(resp.Body)
    if err != nil {
        panic(err)
    }

    fmt.Println(string(body))
}
```

## Sâu hơn
Gói `net/http` trong Go cung cấp một cách mạnh mẽ và linh hoạt để tương tác với các máy chủ HTTP. Thiết kế của nó phản ánh nhấn mạnh của Go vào sự đơn giản, hiệu quả, và độ bền. Ban đầu, các chức năng như xử lý tải trọng JSON hoặc XML yêu cầu việc tự mình tạo ra body của yêu cầu và thiết lập các headers phù hợp. Khi Go phát triển, cộng đồng đã phát triển các gói ở cấp độ cao hơn giúp đơn giản hóa những công việc này, chẳng hạn như `gorilla/mux` cho việc định tuyến và `gjson` cho việc thao tác JSON.

Một điểm đáng chú ý của khách hàng HTTP Go là việc sử dụng giao diện và cấu trúc, như `http.Client` và `http.Request`, cho phép tùy chỉnh và kiểm thử rộng rãi. Ví dụ, bạn có thể chỉnh sửa `http.Client` để hết thời gian yêu cầu hoặc giữ kết nối sống cho hiệu suất.

Một lựa chọn thay thế được xem xét cho các tương tác HTTP đơn giản hơn là sử dụng các thư viện bên thứ ba như "Resty" hoặc "Gentleman." Những gói này cung cấp một trừu tượng cấp cao hơn cho yêu cầu HTTP, làm cho các tác vụ phổ biến trở nên ngắn gọn hơn. Tuy nhiên, việc hiểu và sử dụng gói `net/http` cơ bản là rất quan trọng để xử lý các kịch bản tương tác HTTP phức tạp hoặc độc đáo hơn, cung cấp một nền tảng mà các tính năng đồng thời của Go và thư viện chuẩn mạnh mẽ có thể được khai thác triệt để.
