---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:49.157163-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Elm kh\xF4ng c\xF3 t\xEDnh n\u0103ng ph\xE2\
  n t\xEDch YAML t\xEDch h\u1EE3p s\u1EB5n, v\xEC v\u1EADy b\u1EA1n th\u01B0\u1EDD\
  ng chuy\u1EC3n \u0111\u1ED5i YAML sang JSON b\u1EB1ng m\u1ED9t c\xF4ng c\u1EE5 b\xEA\
  n ngo\xE0i v\xE0 sau \u0111\xF3 l\xE0m\u2026"
lastmod: '2024-03-13T22:44:36.571858-06:00'
model: gpt-4-0125-preview
summary: "Elm kh\xF4ng c\xF3 t\xEDnh n\u0103ng ph\xE2n t\xEDch YAML t\xEDch h\u1EE3\
  p s\u1EB5n, v\xEC v\u1EADy b\u1EA1n th\u01B0\u1EDDng chuy\u1EC3n \u0111\u1ED5i YAML\
  \ sang JSON b\u1EB1ng m\u1ED9t c\xF4ng c\u1EE5 b\xEAn ngo\xE0i v\xE0 sau \u0111\xF3\
  \ l\xE0m vi\u1EC7c v\u1EDBi n\xF3 trong Elm b\u1EB1ng th\u01B0 vi\u1EC7n `elm/json`."
title: "L\xE0m vi\u1EC7c v\u1EDBi YAML"
weight: 41
---

## Cách thực hiện:
Elm không có tính năng phân tích YAML tích hợp sẵn, vì vậy bạn thường chuyển đổi YAML sang JSON bằng một công cụ bên ngoài và sau đó làm việc với nó trong Elm bằng thư viện `elm/json`.

```elm
import Json.Decode exposing (Decoder, field, string, int, decodeValue)

type alias NgườiDùng =
    { tên : String
    , tuổi : Int
    }

bộGiảiMãNgườiDùng : Decoder NgườiDùng
bộGiảiMãNgườiDùng =
    Json.Decode.map2 NgườiDùng
        (field "tên" string)
        (field "tuổi" int)

chuỗiJson : String
chuỗiJson =
    """
    {
        "tên": "Jane Doe",
        "tuổi": 25
    }
    """

kếtQuảPhânTích : Result String NgườiDùng
kếtQuảPhânTích =
    chuỗiJson
        |> Json.Decode.decodeString bộGiảiMãNgườiDùng

-- Đầu ra mẫu: Result.Ok { tên = "Jane Doe", tuổi = 25 }
```
Mã Elm xử lý JSON, tương đương với YAML của bạn sau khi chuyển đổi.

## Sâu hơn:
Sự đơn giản của YAML có nguồn gốc từ đầu những năm 2000 như một lựa chọn thay thế XML dễ đọc cho con người. Mặc dù Elm không phân tích YAML một cách tự nhiên, việc xử lí JSON lại rất dễ dàng, nhờ vào `elm/json`. Một số người sử dụng các dịch vụ hoặc công cụ của bên thứ ba như `yaml-to-json.com` hoặc thậm chí viết một ít mã phía máy chủ trong Node.js hoặc Python để thực hiện việc chuyển đổi từ YAML sang JSON. Nhớ lại rằng, Elm thực sự nổi bật khi làm việc với JSON, vì vậy cách chuyển đổi hai bước này là cách giải quyết mà cộng đồng Elm nói chung sử dụng.

## Xem thêm:
- Gói JSON của Elm: https://package.elm-lang.org/packages/elm/json/latest/
- Trình chuyển đổi YAML sang JSON trực tuyến: https://yaml-to-json.com/
- Trình tạo kiểu Elm từ JSON: https://noredink.github.io/json-to-elm/
