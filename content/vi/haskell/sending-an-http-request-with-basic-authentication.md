---
title:                "Gửi một yêu cầu HTTP với xác thực cơ bản"
date:                  2024-01-28T22:08:20.542826-07:00
model:                 gpt-4-0125-preview
simple_title:         "Gửi một yêu cầu HTTP với xác thực cơ bản"

category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/haskell/sending-an-http-request-with-basic-authentication.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gửi một yêu cầu HTTP với xác thực cơ bản bằng Haskell

## Làm gì & Tại sao?
Gửi một yêu cầu HTTP với xác thực cơ bản có nghĩa là chương trình của bạn gõ cửa dịch vụ web, truyền tài khoản người dùng và mật khẩu để vào. Các lập trình viên thực hiện điều này để truy cập vào các API không mở cho công chúng hoặc để thực hiện các hành động thay mặt cho người dùng.

## Cách thực hiện:
Bạn sẽ cần đến gói `http-conduit` cho các hành động HTTP và `base64-bytestring` để mã hóa thông tin đăng nhập. Nhập chúng và sử dụng `applyBasicAuth` để thêm thông tin đăng nhập vào yêu cầu của bạn.

```Haskell
import Network.HTTP.Simple
import Data.ByteString.Char8 (pack)
import Data.ByteString.Base64 (encode)

-- Xây dựng tiêu đề xác thực cơ bản
let username = "user"
let password = "pass"
let auth = encode $ pack (username ++ ":" ++ password)

-- Tạo yêu cầu của bạn
request' = parseRequest_ "GET http://example.com/bi-mat"
let request = setRequestHeader "Authorization" ["Basic " <> auth] request'

-- Thực hiện yêu cầu
response <- httpLBS request

-- Xử lý phản hồi
print $ getResponseBody response
```

Điều này sẽ xuất ra phản hồi từ API, nếu thông tin đăng nhập của bạn được xác minh.

## Đào sâu
Xác thực cơ bản là cổ xưa trong các năm của web, được thiết kế vào đầu những năm '90, và nó đơn giản nhất có thể: mã hóa base64 `tên người dùng:mật khẩu` được gửi trong một tiêu đề. Nó thiếu các tính năng tiện ích như thời gian hết hạn của mã token và, vì không được mã hóa, nên luôn được sử dụng qua HTTPS.

Các phương thức thay thế như OAuth cung cấp kiểm soát chặt chẽ, an toàn hơn. Đối với Haskell, các thư viện như `http-client` và `wreq` mang lại cho bạn nhiều lựa chọn và linh hoạt hơn.

Về việc triển khai, nhớ không mã hóa cứng thông tin đăng nhập! Sử dụng biến môi trường hoặc một két sắt an toàn trong sản xuất. Và vì mã hóa `base64` không phải là mã hóa (ai cũng có thể giải mã nó), HTTPS không chỉ là một ý tưởng hay, mà còn là một yêu cầu bắt buộc.

## Xem thêm
- Tài liệu Haskell `http-conduit`: https://hackage.haskell.org/package/http-conduit
- `base64-bytestring` cho mã hóa: https://hackage.haskell.org/package/base64-bytestring
- Để bảo mật chặt chẽ, đọc về OAuth2 trong Haskell: https://hackage.haskell.org/package/hoauth2
- Đọc về các phương pháp tốt nhất cho việc lưu trữ bí mật: https://www.yesodweb.com/book/security-considerations
