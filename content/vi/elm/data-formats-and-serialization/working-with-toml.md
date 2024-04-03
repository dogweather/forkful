---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:38.695104-07:00
description: "TOML, vi\u1EBFt t\u1EAFt c\u1EE7a Tom's Obvious, Minimal Language, l\xE0\
  \ m\u1ED9t ng\xF4n ng\u1EEF tu\u1EA7n t\u1EF1 h\xF3a d\u1EEF li\u1EC7u. L\u1EAD\
  p tr\xECnh vi\xEAn Elm s\u1EED d\u1EE5ng n\xF3 \u0111\u1EC3 qu\u1EA3n l\xFD d\u1EEF\
  \ li\u1EC7u c\u1EA5u h\xECnh b\u1EDFi v\xEC\u2026"
lastmod: '2024-03-13T22:44:36.575647-06:00'
model: gpt-4-0125-preview
summary: "TOML, vi\u1EBFt t\u1EAFt c\u1EE7a Tom's Obvious, Minimal Language, l\xE0\
  \ m\u1ED9t ng\xF4n ng\u1EEF tu\u1EA7n t\u1EF1 h\xF3a d\u1EEF li\u1EC7u."
title: "L\xE0m vi\u1EC7c v\u1EDBi TOML"
weight: 39
---

## Làm thế nào:
Elm không có bộ phân tích cú pháp TOML tích hợp sẵn, nhưng bạn có thể tương tác với JavaScript hoặc sử dụng một gói của cộng đồng. Dưới đây là cách bạn có thể phân tích cú pháp TOML bằng một gói `elm-toml` giả định:

```elm
import Toml

configToml : String
configToml =
    """
    [server]
    port = 8080
    """

parseResult : Result Toml.Decode.Error Toml.Value
parseResult =
    Toml.decodeString configToml
```

Để giải mã các giá trị cụ thể:

```elm
portDecoder : Toml.Decode.Decoder Int
portDecoder =
    Toml.Decode.field "server" (Toml.Decode.field "port" Toml.Decode.int)

port : Result String Int
port =
    Toml.decodeString portDecoder configToml
```

Kết quả mẫu cho `port` có thể là `Ok 8080` nếu việc giải mã thành công.

## Tìm hiểu sâu hơn
TOML được tạo ra bởi Tom Preston-Werner, đồng sáng lập của GitHub, như một ngôn ngữ đơn giản cho các tệp cấu hình. Nó cạnh tranh với YAML và JSON; cú pháp của TOML nhằm mục đích lấy điều tốt nhất của cả hai thế giới với trọng tâm là dễ đọc và viết cho con người.

Trong Elm, để xử lý TOML, bạn thường cần phải qua tương tác với JavaScript, có thể hơi rườm rà một chút. May mắn thay, cộng đồng Elm rất giàu nguồn lực, và có nhiều gói bên thứ ba tồn tại. Gói `elm-toml` giả định sẽ có khả năng sử dụng `Port` của Elm để nói chuyện với một bộ phân tích cú pháp TOML bằng JavaScript hoặc triển khai việc phân tích cú pháp trực tiếp trong Elm.

Rào cản chính trong Elm là tất cả mọi thứ đều được kiểu định tĩnh, vì vậy bạn sẽ cần phải viết các bộ giải mã tùy chỉnh để xử lý các cấu trúc dữ liệu khác nhau trong TOML, có thể hơi rườm rà nhưng tăng thêm sự an toàn.

## Xem thêm
Để biết thông số và thông tin thêm về chính TOML, hãy xem tại [TOML](https://toml.io).
Nếu bạn đang tìm kiếm một cách tiếp cận thực hành với Elm và tương tác JavaScript, hãy bắt đầu với hướng dẫn chính thức: [Cổng Elm](https://guide.elm-lang.org/interop/ports.html).
Để duyệt các gói cộng đồng hoặc đóng góp, hãy xem [Gói Elm](https://package.elm-lang.org/).
