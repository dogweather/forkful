---
title:                "Tải trang web"
date:                  2024-01-28T22:00:00.953206-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tải trang web"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/elm/downloading-a-web-page.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?

Tải một trang web nghĩa là thu thập dữ liệu từ internet trực tiếp vào ứng dụng của bạn để hiển thị hoặc xử lý nó. Lập trình viên làm điều này để lấy thông tin thời gian thực hoặc cung cấp nội dung động cho người dùng.

## Làm thế nào:

Elm yêu cầu các hiệu ứng phụ như các yêu cầu HTTP được cấu trúc như các lệnh. Bạn sẽ sử dụng mô-đun `Http` để lấy và xử lý phản hồi.

```Elm

module Main exposing (main)

import Browser
import Html exposing (Html, text)
import Http

type alias Model =
    { content : String }

type Msg
    = GotText (Result Http.Error String)

init : ( Model, Cmd Msg )
init =
    ( Model ""
    , fetchPage "https://api.example.com/data"
    )

fetchPage : String -> Cmd Msg
fetchPage url =
    Http.get { url = url, expect = Http.expectString GotText }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    trường hợp msg của
        GotText (Ok data) ->
            ( { model | content = data }, Cmd.none )

        GotText (Err _) ->
            ( { model | content = "Lỗi: Không thể tải trang." }, Cmd.none )

view : Model -> Html Msg
view model =
    text model.content

main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }

```

Nếu việc tải thành công, `content` trong mô hình của bạn sẽ chứa nội dung của trang. Nếu có lỗi, nó sẽ chứa một thông điệp lỗi đơn giản.

## Sâu hơn

Elm coi hiệu ứng phụ như Dữ liệu, có nghĩa là các yêu cầu HTTP được quản lý bởi thời gian chạy Elm, không phải trực tiếp trong mã của bạn. Trong lịch sử, đây là một sự khác biệt so với các ngôn ngữ như JavaScript, nơi hiệu ứng phụ tự do hơn. Các phương án thay thế trong các ngôn ngữ khác có thể là `fetch` trong JavaScript hoặc `requests` của Python. Cách tiếp cận của Elm đảm bảo ứng dụng của bạn vẫn dự đoán được và dễ bảo trì bằng cách mã hóa các hiệu ứng phụ thành các kiểu và sử dụng một hàm `update` tập trung để quản lý thay đổi.

Mô-đun `Http` không luôn tồn tại trong Elm. Các phiên bản đầu tiên của Elm thực hiện AJAX của riêng mình, điều này gây ra sự cồng kềnh. Bây giờ, `Http` cung cấp một bộ hàm để xử lý các trường hợp khác nhau, như mong đợi JSON hoặc chuỗi, làm cho nó thân thiện với người dùng hơn.

Về mặt thực hiện, khi bạn gọi `fetchPage`, Elm gửi một thông điệp đến hàm `update` của bạn với kết quả. Nó sẽ là `Ok data` nếu thành công hoặc `Err error` nếu thất bại. Bạn so khớp với những kết quả này và cập nhật `Model` và giao diện của bạn theo đó.

## Xem thêm

- Tài liệu về gói HTTP của Elm: [https://package.elm-lang.org/packages/elm/http/latest/](https://package.elm-lang.org/packages/elm/http/latest/)
- Hướng dẫn Elm về Hiệu ứng: [https://guide.elm-lang.org/effects/](https://guide.elm-lang.org/effects/)
- Giải mã JSON trong Elm (khi dữ liệu bạn tải không phải là một chuỗi đơn giản): [https://package.elm-lang.org/packages/elm/json/latest/](https://package.elm-lang.org/packages/elm/json/latest/)
