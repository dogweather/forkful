---
title:                "Làm việc với TOML"
aliases:
- vi/elm/working-with-toml.md
date:                  2024-01-28T22:11:38.695104-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/elm/working-with-toml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
TOML, viết tắt của Tom's Obvious, Minimal Language, là một ngôn ngữ tuần tự hóa dữ liệu. Lập trình viên Elm sử dụng nó để quản lý dữ liệu cấu hình bởi vì nó dễ đọc cho con người và ánh xạ gọn gàng với các cặp khóa-giá trị cần thiết trong ứng dụng.

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
