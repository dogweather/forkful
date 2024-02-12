---
title:                "Gửi một yêu cầu HTTP với xác thực cơ bản"
aliases:
- vi/clojure/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-28T22:08:13.084190-07:00
model:                 gpt-4-0125-preview
simple_title:         "Gửi một yêu cầu HTTP với xác thực cơ bản"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/clojure/sending-an-http-request-with-basic-authentication.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?

Gửi một yêu cầu HTTP với xác thực cơ bản bao gồm việc thêm tên người dùng và mật khẩu vào một yêu cầu để truy cập vào các tài nguyên bị hạn chế. Các lập trình viên thực hiện điều này để truy cập vào các API hoặc dịch vụ web đòi hỏi một số mức độ bảo mật.

## Cách thực hiện:

Trong Clojure, bạn sẽ thường sử dụng thư viện `clj-http` cho các yêu cầu HTTP, bao gồm cả những yêu cầu với xác thực cơ bản. Hãy bắt đầu bằng cách thêm dependency (`[clj-http "3.12.3"]` tính đến lần cập nhật cuối cùng của tôi) vào `project.clj` của bạn.

Tiếp theo, dưới đây là cách bạn tạo một yêu cầu GET với xác thực cơ bản:

```clojure
(require '[clj-http.client :as client])

(let [response (client/get "https://your-api.com/resource"
                           {:basic-auth ["username" "password"]})]
  (println "Trạng thái:" (:status response))
  (println "Nội dung:" (:body response)))
```
Thay `"https://your-api.com/resource"`, `"username"`, và `"password"` bằng thông tin của bạn. Mã này gửi một yêu cầu GET và in ra trạng thái và nội dung của phản hồi.

Kết quả mẫu có thể trông như thế này:

```
Trạng thái: 200
Nội dung: {Dữ liệu JSON hoặc cái gì đó khác ở đây}
```

## Sâu hơn

Xác thực cơ bản HTTP có nguồn gốc từ các giao thức web sơ khai. Nó truyền tên người dùng và mật khẩu trong một tiêu đề HTTP được mã hóa sử dụng Base64. Mặc dù nó đơn giản, nhưng không phải là an toàn nhất vì thông tin đăng nhập có thể dễ dàng được giải mã nếu bị ngắt quãng.

Các phương thức thay thế:
- **Xác thực Digest**: Phức tạp hơn, liên quan đến việc gửi một phiên bản đã được băm của mật khẩu thay vì.
- **OAuth**: Một hệ thống xác thực mạnh mẽ hơn không đòi hỏi việc gửi tên người dùng và mật khẩu.
- **Khóa API**: Token duy nhất được sử dụng thay cho thông tin đăng nhập truyền thống.

Bên dưới phần thư viện `clj-http`, chỉ định `:basic-auth` trong hashmap tùy chọn kích hoạt thư viện mã hóa thông tin đăng nhập của bạn và đính kèm chúng vào tiêu đề HTTP `Authorization`. Khi máy chủ nhận được yêu cầu, nó giải mã tiêu đề và kiểm tra thông tin đăng nhập.

Hãy nhớ rằng, để truyền tải an toàn, HTTPS nên được sử dụng để ngăn chặn người khác chặn thông tin đăng nhập của bạn.

## Xem thêm

- Kho GitHub clj-http: https://github.com/dakrone/clj-http
- Tài liệu chính thức của Clojure: https://clojure.org/
- Xác thực HTTP trên MDN: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication 
- Hiểu về OAuth: https://oauth.net/
