---
changelog:
- 2024-02-27, dogweather, edited and tested
- 2024-02-27, gpt-4-0125-preview, translated from English
date: 2024-02-27 22:50:22.468138-07:00
description: "Vi\u1EC7c t\u1EA1o s\u1ED1 ng\u1EABu nhi\xEAn trong Elm bao g\u1ED3\
  m vi\u1EC7c s\u1EED d\u1EE5ng module `Random` \u0111\u1EC3 s\u1EA3n xu\u1EA5t s\u1ED1\
  \ gi\u1EA3 ng\u1EABu nhi\xEAn, r\u1EA5t h\u1EEFu \xEDch cho nhi\u1EC1u t\xE1c v\u1EE5\
  \ nh\u01B0 tr\xF2 ch\u01A1i, m\xF4 ph\u1ECFng,\u2026"
lastmod: '2024-03-13T22:44:36.538505-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c t\u1EA1o s\u1ED1 ng\u1EABu nhi\xEAn trong Elm bao g\u1ED3m vi\u1EC7\
  c s\u1EED d\u1EE5ng module `Random` \u0111\u1EC3 s\u1EA3n xu\u1EA5t s\u1ED1 gi\u1EA3\
  \ ng\u1EABu nhi\xEAn, r\u1EA5t h\u1EEFu \xEDch cho nhi\u1EC1u t\xE1c v\u1EE5 nh\u01B0\
  \ tr\xF2 ch\u01A1i, m\xF4 ph\u1ECFng,\u2026"
title: "Sinh s\u1ED1 ng\u1EABu nhi\xEAn"
weight: 12
---

## Cái gì & Tại sao?
Việc tạo số ngẫu nhiên trong Elm bao gồm việc sử dụng module `Random` để sản xuất số giả ngẫu nhiên, rất hữu ích cho nhiều tác vụ như trò chơi, mô phỏng, và thậm chí là một phần của các thuật toán yêu cầu quá trình ngẫu nhiên học. Khả năng này cho phép các nhà phát triển thêm tính không chắc chắn và đa dạng vào ứng dụng của họ, nâng cao trải nghiệm và chức năng cho người dùng.

## Làm thế nào:
Bản chất hàm mô phỏng thuần túy của Elm nghĩa là bạn không thể tạo số ngẫu nhiên trực tiếp như bạn có thể làm trong các ngôn ngữ mệnh lệnh. Thay vào đó, bạn sử dụng module `Random` kết hợp với các lệnh. Dưới đây là một ví dụ cơ bản tạo một số nguyên ngẫu nhiên từ 1 đến 100.

Đầu tiên, cài đặt module `Random` bằng `elm install elm/random`. Sau đó nhập nó vào tệp Elm của bạn, cùng với các module HTML và sự kiện cần thiết, như sau:

`src/Main.elm`

```elm
module Main exposing (..)

import Browser
import Html exposing (Html, button, text, div)
import Html.Events exposing (onClick)
import Random
```

Để đây là một ví dụ đầy đủ, bạn có thể thêm phần mẫu như sau:
```elm
main =
  Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }

init : () -> (Model, Cmd Msg)
init _ =
  (Model 0, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none
```

Tiếp theo, xác định một **lệnh** để tạo số ngẫu nhiên. Điều này bao gồm thiết lập một kiểu `Msg` để xử lý số ngẫu nhiên một khi nó được tạo, một `Model` để lưu trữ nó, và một hàm cập nhật để kết hợp tất cả.
```elm
type Msg
    = Generate
    | NewRandom Int

type alias Model = { randomNumber : Int }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Generate ->
            ( model, Random.generate NewRandom (Random.int 1 100) )

        NewRandom number ->
            ( { model | randomNumber = number }, Cmd.none )
```

Để kích hoạt việc tạo số, bạn sẽ gửi một tin nhắn `Generate`, ví dụ, thông qua một nút trong giao diện của bạn:
```elm
view : Model -> Html Msg
view model =
    div []
        [ div [] [ text ("Số Ngẫu Nhiên: " ++ String.fromInt model.randomNumber) ]
        , button [ onClick Generate ] [ text "Tạo" ]
        ]
```

Khi bạn nhấn vào nút "Tạo", một số ngẫu nhiên từ 1 đến 100 sẽ được hiển thị.

Cách tiếp cận đơn giản này có thể được điều chỉnh và mở rộng, tận dụng các hàm khác trong module `Random` để tạo số thực ngẫu nhiên, danh sách, hoặc thậm chí là các cấu trúc dữ liệu phức tạp dựa trên kiểu tùy chỉnh, cung cấp một sân chơi rộng lớn để thêm tính không chắc chắn vào ứng dụng Elm của bạn.

Hướng dẫn Elm đi vào chi tiết nhiều hơn. Nó cũng có [một ví dụ về việc lắc một xúc xắc sáu mặt](https://guide.elm-lang.org/effects/random).
