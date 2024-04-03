---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:59.326658-07:00
description: "L\xE0m th\u1EBF n\xE0o: Elm ch\u1EE7 y\u1EBFu t\u1EADp trung v\xE0o\
  \ ph\xE1t tri\u1EC3n web front-end, n\u01A1i m\xE0 truy c\u1EADp tr\u1EF1c ti\u1EBF\
  p v\xE0o h\u1EC7 th\u1ED1ng t\u1EC7p l\xE0 kh\xF4ng th\u1EC3 v\xEC l\xFD do an ninh.\
  \ Thay v\xE0o \u0111\xF3,\u2026"
lastmod: '2024-03-13T22:44:36.567632-06:00'
model: gpt-4-0125-preview
summary: "Elm ch\u1EE7 y\u1EBFu t\u1EADp trung v\xE0o ph\xE1t tri\u1EC3n web front-end,\
  \ n\u01A1i m\xE0 truy c\u1EADp tr\u1EF1c ti\u1EBFp v\xE0o h\u1EC7 th\u1ED1ng t\u1EC7\
  p l\xE0 kh\xF4ng th\u1EC3 v\xEC l\xFD do an ninh."
title: "\u0110\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n"
weight: 22
---

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
