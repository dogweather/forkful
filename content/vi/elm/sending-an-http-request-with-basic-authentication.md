---
title:                "Gửi một yêu cầu HTTP với xác thực cơ bản"
date:                  2024-01-28T22:09:23.390678-07:00
model:                 gpt-4-0125-preview
simple_title:         "Gửi một yêu cầu HTTP với xác thực cơ bản"

category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/elm/sending-an-http-request-with-basic-authentication.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?

Gửi một yêu cầu HTTP với xác thực cơ bản bao gồm việc đính kèm thông tin đăng nhập (tên đăng nhập và mật khẩu) vào tiêu đề yêu cầu để truy cập các tài nguyên được bảo vệ. Các lập trình viên sử dụng nó cho việc xác thực đơn giản trên các API HTTP nơi mà việc mang theo overhead của các hệ thống phức tạp hơn không cần thiết.

## Làm thế nào:

Elm tạo yêu cầu HTTP sử dụng gói `Http`. Để thêm xác thực cơ bản, bạn mã hóa thông tin đăng nhập và bao gồm chúng trong tiêu đề yêu cầu.

```Elm
import Http
import Base64

type alias Model = { ... }
type Msg = HttpRequestCompleted (Result Http.Error String)

-- Mã hóa tên đăng nhập và mật khẩu
basicAuthHeader : String -> String -> Http.Header
basicAuthHeader username password =
    let
        credentials = username ++ ":" ++ password
        encodedCredentials = Base64.encode credentials
    in
    Http.header "Authorization" ("Basic " ++ encodedCredentials)

-- Thực hiện yêu cầu HTTP
sendRequestWithBasicAuth : Cmd Msg
sendRequestWithBasicAuth =
    let
        url = "https://example.com/protected/resource"
        request =
            Http.request
                { method = "GET"
                , headers = [ basicAuthHeader "myUsername" "myPassword" ]
                , url = url
                , body = Http.emptyBody
                , expect = Http.expectString (HttpRequestCompleted)
                , timeout = Nothing
                , tracker = Nothing
                }
    in
    Http.send HttpRequestCompleted request
```

Khi hàm trên được gọi, Elm sẽ thực hiện một yêu cầu GET đến URL đã chỉ định với tiêu đề Authorization được thiết lập là tên đăng nhập và mật khẩu đã mã hóa.

## Tìm hiểu sâu hơn

Cách tiếp cận của Elm đối với yêu cầu HTTP phản ánh triết lý chung của ngôn ngữ: an toàn, dễ bảo trì và dễ hiểu. Gói `Http` bao bọc yêu cầu theo cách xử lý với kiến trúc Elm.

Xác thực cơ bản cũ như chính web vậy, là một phần của quy specification HTTP ban đầu (RFC 7617). Nó đơn giản nhưng không rất an toàn vì thông tin đăng nhập chỉ được mã hóa base64, không được mã hóa. Vì vậy, rất quan trọng phải sử dụng HTTPS để mã hóa truyền dẫn.

Các phương án thay thế cho xác thực cơ bản bao gồm OAuth, tokens như JWT, hoặc API keys, mỗi phương án đều đem lại độ phức tạp và bảo mật được cải thiện hơn. Elm cũng hỗ trợ những phương pháp này nhưng thường yêu cầu các gói bổ sung hoặc bộ mã hóa và giải mã tùy chỉnh.

## Xem thêm

- Tài liệu gói `Http` chính thức của Elm: [package.elm-lang.org/packages/elm/http/latest](https://package.elm-lang.org/packages/elm/http/latest)
- Nguồn gói `Base64` của Elm: [package.elm-lang.org/packages/truqu/elm-base64/latest](https://package.elm-lang.org/packages/truqu/elm-base64/latest)
- RFC 7617, Chế độ xác thực HTTP cơ bản: [tools.ietf.org/html/rfc7617](https://tools.ietf.org/html/rfc7617)
