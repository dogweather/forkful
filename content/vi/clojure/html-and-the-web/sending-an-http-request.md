---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:29.951702-07:00
description: "L\xE0m Sao: Trong Clojure, b\u1EA1n c\xF3 th\u1EC3 g\u1EEDi y\xEAu c\u1EA7\
  u HTTP s\u1EED d\u1EE5ng kh\xE1ch h\xE0ng `clj-http`. \u0110\u1EA7u ti\xEAn, th\xEA\
  m ph\u1EE5 thu\u1ED9c v\xE0o `project.clj` c\u1EE7a b\u1EA1n."
lastmod: '2024-03-13T22:44:36.148810-06:00'
model: gpt-4-0125-preview
summary: "Trong Clojure, b\u1EA1n c\xF3 th\u1EC3 g\u1EEDi y\xEAu c\u1EA7u HTTP s\u1EED\
  \ d\u1EE5ng kh\xE1ch h\xE0ng `clj-http`."
title: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP"
weight: 44
---

## Làm Sao:
Trong Clojure, bạn có thể gửi yêu cầu HTTP sử dụng khách hàng `clj-http`.

Đầu tiên, thêm phụ thuộc vào `project.clj` của bạn:
```clojure
[clj-http "3.12.3"]
```

Bây giờ, hãy gửi một yêu cầu GET:
```clojure
(require '[clj-http.client :as client])

(let [response (client/get "http://httpbin.org/get")]
  (println response))
```

Mẫu đầu ra:
```clojure
{:status 200, :headers {...}, :body "..."}
```

Để đăng dữ liệu:
```clojure
(let [response (client/post "http://httpbin.org/post" {:form-params {:key "value"}})]
  (println response))
```

## Đào Sâu
Gửi yêu cầu HTTP không phải là điều mới. Nó cũ như chính web. Clojure, với tư cách là một Lisp hiện đại, có một số thư viện để thực hiện yêu cầu HTTP. `clj-http` là một trong những thư viện phổ biến, nhưng cũng có những thư viện khác như `http-kit` hoặc `clj-http.client` cốt lõi của Clojure.

`clj-http` dựa trên Apache HttpComponents Client cho Java ở phía dưới. Nó đa năng nhưng có thể cảm thấy nặng về Java. Một lựa chọn khác, `http-kit`, nhẹ hơn và theo phong cách Clojure hơn nhưng không giàu tính năng.

Khi bạn gửi yêu cầu HTTP, bạn làm điều đó qua TCP/IP, mà đóng gói các yêu cầu của bạn theo một giao thức đã được thiết lập rõ ràng. Chuẩn mực toàn cầu này cho phép bạn tương tác với hầu như bất kỳ dịch vụ web nào.

## Xem Thêm
- Kho lưu trữ GitHub của `clj-http`: https://github.com/dakrone/clj-http
- Trang chính thức của Clojure: https://clojure.org
- Tài liệu của HttpComponents Client: https://hc.apache.org/httpcomponents-client-ga/
- Đối với nhu cầu thời gian thực, xem xét `http-kit`: http://www.http-kit.org
