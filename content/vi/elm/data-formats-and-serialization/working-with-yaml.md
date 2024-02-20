---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:49.157163-07:00
description: "YAML, m\u1ED9t ti\xEAu chu\u1EA9n m\xE3 h\xF3a d\u1EEF li\u1EC7u th\xE2\
  n thi\u1EC7n v\u1EDBi con ng\u01B0\u1EDDi, \u0111\u01B0\u1EE3c s\u1EED d\u1EE5ng\
  \ cho c\xE1c file c\u1EA5u h\xECnh v\xE0 trao \u0111\u1ED5i d\u1EEF li\u1EC7u. C\xE1\
  c l\u1EADp tr\xECnh vi\xEAn \u01B0a th\xEDch n\xF3 v\xEC n\xF3\u2026"
lastmod: 2024-02-19 22:04:55.741023
model: gpt-4-0125-preview
summary: "YAML, m\u1ED9t ti\xEAu chu\u1EA9n m\xE3 h\xF3a d\u1EEF li\u1EC7u th\xE2\
  n thi\u1EC7n v\u1EDBi con ng\u01B0\u1EDDi, \u0111\u01B0\u1EE3c s\u1EED d\u1EE5ng\
  \ cho c\xE1c file c\u1EA5u h\xECnh v\xE0 trao \u0111\u1ED5i d\u1EEF li\u1EC7u. C\xE1\
  c l\u1EADp tr\xECnh vi\xEAn \u01B0a th\xEDch n\xF3 v\xEC n\xF3\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi YAML"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

YAML, một tiêu chuẩn mã hóa dữ liệu thân thiện với con người, được sử dụng cho các file cấu hình và trao đổi dữ liệu. Các lập trình viên ưa thích nó vì nó rõ ràng, dễ đọc và được áp dụng rộng rãi trên các công cụ và ngôn ngữ.

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
