---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:23.390678-07:00
description: "L\xE0m th\u1EBF n\xE0o: Elm t\u1EA1o y\xEAu c\u1EA7u HTTP s\u1EED d\u1EE5\
  ng g\xF3i `Http`. \u0110\u1EC3 th\xEAm x\xE1c th\u1EF1c c\u01A1 b\u1EA3n, b\u1EA1\
  n m\xE3 h\xF3a th\xF4ng tin \u0111\u0103ng nh\u1EADp v\xE0 bao g\u1ED3m ch\xFAng\
  \ trong ti\xEAu \u0111\u1EC1 y\xEAu c\u1EA7u."
lastmod: '2024-03-13T22:44:36.543771-06:00'
model: gpt-4-0125-preview
summary: "Elm t\u1EA1o y\xEAu c\u1EA7u HTTP s\u1EED d\u1EE5ng g\xF3i `Http`."
title: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP v\u1EDBi x\xE1c th\u1EF1c c\u01A1 b\u1EA3\
  n"
weight: 45
---

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
