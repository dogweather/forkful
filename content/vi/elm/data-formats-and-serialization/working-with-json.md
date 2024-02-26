---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:27.820988-07:00
description: "JSON (JavaScript Object Notation) l\xE0 m\u1ED9t \u0111\u1ECBnh d\u1EA1\
  ng v\u0103n b\u1EA3n cho vi\u1EC7c trao \u0111\u1ED5i d\u1EEF li\u1EC7u, t\u01B0\
  \u01A1ng t\u1EF1 nh\u01B0 XML nh\u01B0ng nh\u1EB9 h\u01A1n v\xE0 d\u1EC5 \u0111\u1ECD\
  c h\u01A1n. C\xE1c l\u1EADp tr\xECnh vi\xEAn Elm\u2026"
lastmod: '2024-02-25T18:49:34.914025-07:00'
model: gpt-4-0125-preview
summary: "JSON (JavaScript Object Notation) l\xE0 m\u1ED9t \u0111\u1ECBnh d\u1EA1\
  ng v\u0103n b\u1EA3n cho vi\u1EC7c trao \u0111\u1ED5i d\u1EEF li\u1EC7u, t\u01B0\
  \u01A1ng t\u1EF1 nh\u01B0 XML nh\u01B0ng nh\u1EB9 h\u01A1n v\xE0 d\u1EC5 \u0111\u1ECD\
  c h\u01A1n. C\xE1c l\u1EADp tr\xECnh vi\xEAn Elm\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi JSON"
---

{{< edit_this_page >}}

## Gì và Tại Sao?

JSON (JavaScript Object Notation) là một định dạng văn bản cho việc trao đổi dữ liệu, tương tự như XML nhưng nhẹ hơn và dễ đọc hơn. Các lập trình viên Elm sử dụng JSON để gửi và nhận dữ liệu từ/như máy chủ, tạo ra các ứng dụng web đầy động và dựa trên dữ liệu.

## Cách thực hiện:

Elm xử lý JSON sử dụng các module `Json.Decode` và `Json.Encode`. Dưới đây là một ví dụ cơ bản:

```Elm
import Html exposing (text)
import Json.Decode exposing (string)

-- Giải mã một chuỗi JSON đơn giản
jsonString : String
jsonString = "{\"name\": \"Elm\"}"

type alias User =
    { name : String }

userNameDecoder : Json.Decode.Decoder String
userNameDecoder =
    Json.Decode.field "name" string

main =
    case Json.Decode.decodeString userNameDecoder jsonString of
        Ok name ->
            text ("Chào mừng, " ++ name)

        Err _ ->
            text "Rất tiếc, có lỗi xảy ra!"
```
Đầu ra: 
```
Chào mừng, Elm
```

## Sâu hơn

JSON đã trở thành tiêu chuẩn de facto cho các API web kể từ đầu những năm 2000, thay thế XML bởi sự đơn giản của nó. Mặc dù Elm được biết đến với tính ngắn gọn và an toàn về kiểu dữ liệu, việc xử lý JSON có thể trở nên rườm rà do nhu cầu phải có các bộ giải mã rõ ràng.

Các lựa chọn khác như Haskell sử dụng typeclasses cho việc mã hóa/giải mã JSON, cung cấp nhiều chức năng "thuận tiện" hơn ngay từ đầu. Tuy nhiên, cách tiếp cận của Elm giúp duy trì an toàn về kiểu dữ liệu và tránh lỗi thời gian chạy. Các bộ giải mã chỉ rõ cách để chuyển đổi JSON thành kiểu dữ liệu của Elm, và bộ mã hóa thực hiện quá trình ngược lại.

## Xem thêm

Để biết thêm thông tin và tài liệu:

- Hướng dẫn JSON chính thức của Elm: [Làm việc với JSON trong Elm](https://guide.elm-lang.org/effects/json.html)
- Tài liệu Json.Decode: [Elm Json.Decode](https://package.elm-lang.org/packages/elm/json/latest/Json-Decode)
- Tài liệu Json.Encode: [Elm Json.Encode](https://package.elm-lang.org/packages/elm/json/latest/Json-Encode)
