---
title:                "Đọc một tệp văn bản"
date:                  2024-01-28T22:04:59.326658-07:00
model:                 gpt-4-0125-preview
simple_title:         "Đọc một tệp văn bản"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/elm/reading-a-text-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Đọc một tệp văn bản là lấy nội dung từ một tệp được cấu trúc dưới dạng văn bản đọc được, thay vì dữ liệu nhị phân. Các lập trình viên đọc tệp văn bản để truy cập dữ liệu, cấu hình, hoặc để nhập khẩu lượng lớn văn bản vào ứng dụng của họ.

## Làm thế nào:

Elm chủ yếu tập trung vào phát triển web front-end, nơi mà truy cập trực tiếp vào hệ thống tệp là không thể vì lý do an ninh. Thay vào đó, bạn xử lý việc tải tệp lên bởi người dùng. Dưới đây là cách bạn có thể đọc một tệp văn bản mà người dùng chọn:

```Elm
module Main exposing (..)

import Browser
import File exposing (File)
import File.Selector as Selector
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

type alias Model =
    { fileContent : String }

type Msg
    = SelectFile
    | ReceiveFileContent (Result () String)

init : Model
init =
    { fileContent = "" }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SelectFile ->
            (model, fileSelectCmd)

        ReceiveFileContent (Ok content) ->
            ({ model | fileContent = content }, Cmd.none)

        ReceiveFileContent (Err _) ->
            (model, Cmd.none)

fileSelectCmd : Cmd Msg
fileSelectCmd =
    File.select [ Selector.accept "text/*" ] { onDone = ReceiveFileContent }

view : Model -> Html Msg
view model =
    div []
        [ button [ onClick SelectFile ] [ text "Chọn một tệp văn bản" ]
        , div [] [ text model.fileContent ]
        ]

main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }
```

Chạy code trong trình duyệt của bạn, nhấp vào nút và chọn một tệp văn bản. Nội dung sẽ hiển thị trong ứng dụng Elm của bạn.

## Đào sâu

Elm không đọc tệp trực tiếp từ hệ thống tệp của máy chủ - nó không được thiết kế cho các hoạt động ở phía máy chủ. Thay vào đó, Elm quản lý nhập tệp thông qua API Tệp trong trình duyệt, thường được kích hoạt bởi một hành động của người dùng, chẳng hạn như việc chọn tệp hoặc hành động kéo và thả. Đó là một biện pháp an ninh.

Trong quá khứ, bạn có thể đã sử dụng JavaScript và Node.js để đọc tệp ở phía máy chủ, hoặc XMLHttpRequest (XHR) để đọc ở phía máy khách mà không cần tương tác của người dùng. Những cái này có các mô hình an ninh và khả năng khác nhau.

Các module `File` và `File.Selector` trong Elm giúp việc xử lý đọc tệp trong trình duyệt khá mượt mà, nhưng hãy nhớ về triết lý "không tác dụng phụ" của Elm. Điều đó có nghĩa là việc đọc tệp được kiểm soát chặt chẽ, yêu cầu các hành động rõ ràng của người dùng. Ngoài ra, việc phân tích và giải mã nội dung tệp cần phải cẩn thận để phù hợp với kiểu mạnh của Elm.

## Xem thêm

- Tài liệu API Tệp Elm chính thức: https://package.elm-lang.org/packages/elm/file/latest/
- Hướng dẫn về các lệnh và đăng ký trong Elm (để hiểu về các hoạt động bất đồng bộ): https://guide.elm-lang.org/effects/
- Elm Discuss cho các câu hỏi và tương tác cộng đồng: https://discourse.elm-lang.org/
