---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:00.169789-07:00
description: "L\xE0m th\u1EBF n\xE0o: \u0110\u01B0\u1EE3c r\u1ED3i, \u0111\u1EBFn\
  \ l\xFAc vi\u1EBFt code. Elm th\u1EF1c hi\u1EC7n y\xEAu c\u1EA7u HTTP s\u1EED d\u1EE5\
  ng m\xF4-\u0111un `Http`. D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9t v\xED d\u1EE5\
  \ nhanh \u0111\u1EC3 t\u1EA3i xu\u1ED1ng m\u1ED9t s\u1ED1 JSON."
lastmod: '2024-03-13T22:44:36.539879-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u01B0\u1EE3c r\u1ED3i, \u0111\u1EBFn l\xFAc vi\u1EBFt code."
title: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP"
weight: 44
---

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
