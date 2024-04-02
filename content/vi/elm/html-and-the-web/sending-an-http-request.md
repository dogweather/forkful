---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:00.169789-07:00
description: "Trong Elm, vi\u1EC7c g\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP l\xE0 c\xE1\
  ch \u1EE9ng d\u1EE5ng c\u1EE7a b\u1EA1n giao ti\u1EBFp v\u1EDBi c\xE1c d\u1ECBch\
  \ v\u1EE5 web kh\xE1c \u0111\u1EC3 trao \u0111\u1ED5i d\u1EEF li\u1EC7u. L\u1EAD\
  p tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 t\u1EA3i\u2026"
lastmod: '2024-03-13T22:44:36.539879-06:00'
model: gpt-4-0125-preview
summary: "Trong Elm, vi\u1EC7c g\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP l\xE0 c\xE1\
  ch \u1EE9ng d\u1EE5ng c\u1EE7a b\u1EA1n giao ti\u1EBFp v\u1EDBi c\xE1c d\u1ECBch\
  \ v\u1EE5 web kh\xE1c \u0111\u1EC3 trao \u0111\u1ED5i d\u1EEF li\u1EC7u. L\u1EAD\
  p tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 t\u1EA3i\u2026"
title: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP"
weight: 44
---

## Gì và Tại sao?

Trong Elm, việc gửi một yêu cầu HTTP là cách ứng dụng của bạn giao tiếp với các dịch vụ web khác để trao đổi dữ liệu. Lập trình viên làm điều này để tải lên hoặc gửi thông tin đến máy chủ, thúc đẩy dinamika của ứng dụng như tài khoản người dùng, điểm số, hoặc cập nhật tin tức.

## Làm thế nào:

Được rồi, đến lúc viết code. Elm thực hiện yêu cầu HTTP sử dụng mô-đun `Http`. Dưới đây là một ví dụ nhanh để tải xuống một số JSON:

```Elm
import Http
import Json.Decode as Decode

type alias User =
    { id : Int
    , username : String
    }

userDecoder : Decode.Decoder User
userDecoder =
    Decode.map2 User
        (Decode.field "id" Decode.int)
        (Decode.field "username" Decode.string)

fetchUser : Cmd Msg
fetchUser =
    Http.get
        { url = "https://api.example.com/user/1"
        , decoder = userDecoder
        }
        |> Http.send UserFetched

type Msg
    = UserFetched (Result Http.Error User)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        UserFetched (Ok user) ->
            ({ model | user = Just user }, Cmd.none)

        UserFetched (Err _) ->
            (model, Cmd.none)
```

Đầu ra mẫu khi `UserFetched` là một `Ok user`:

```Elm
{ id = 1, username = "ElmerFudd" }
```

## Sâu hơn

Việc gửi yêu cầu HTTP không phải là mới; nó đã là cột sống của giao tiếp web kể từ những năm 90. Elm đóng gói sự phức tạp trong mô-đun `Http` thân thiện với người dùng, tập trung vào sự an toàn và đơn giản. Không giống như những ngày đầu, Elm trừu tượng hóa các phần rối rắm như XMLHttprequest và phân tích cú pháp JSON. Các phương pháp thay thế như sử dụng Fetch API hoặc XMLHttpRequest trực tiếp của JavaScript có thể thực hiện được thông qua cổng, nhưng cách tích hợp sẵn của Elm giữ cho mã của bạn an toàn về kiểu và nguyên sơ. Nó xử lý các tác dụng phụ thông qua kiến trúc mạnh mẽ mà không làm tổn hại đến độ tin cậy của ứng dụng của bạn.

## Xem thêm

Để biết thêm giải thích chi tiết và khắc phục sự cố, hãy xem những nguồn lực sau:

- Tài liệu gói Elm cho HTTP: [https://package.elm-lang.org/packages/elm/http/latest/](https://package.elm-lang.org/packages/elm/http/latest/)
- Giải mã JSON trong Elm: [https://package.elm-lang.org/packages/elm/json/latest/](https://package.elm-lang.org/packages/elm/json/latest/)
- Hướng dẫn Elm về yêu cầu HTTP: [https://guide.elm-lang.org/effects/http.html](https://guide.elm-lang.org/effects/http.html)
- Elm Discuss cho cái nhìn của cộng đồng: [https://discourse.elm-lang.org/](https://discourse.elm-lang.org/)
