---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:41.739583-07:00
description: "Vi\u1EC7c t\u1EA1o s\u1ED1 ng\u1EABu nhi\xEAn trong Elm bao g\u1ED3\
  m vi\u1EC7c t\u1EA1o ra nh\u1EEFng gi\xE1 tr\u1ECB s\u1ED1 kh\xF4ng th\u1EC3 \u0111\
  o\xE1n tr\u01B0\u1EDBc \u0111\u01B0\u1EE3c, \u0111\xF3 l\xE0 \u0111i\u1EC1u thi\u1EBF\
  t y\u1EBFu cho c\xE1c \u1EE9ng d\u1EE5ng nh\u01B0 tr\xF2 ch\u01A1i, m\xF4\u2026"
lastmod: '2024-02-25T18:49:34.878430-07:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c t\u1EA1o s\u1ED1 ng\u1EABu nhi\xEAn trong Elm bao g\u1ED3m vi\u1EC7\
  c t\u1EA1o ra nh\u1EEFng gi\xE1 tr\u1ECB s\u1ED1 kh\xF4ng th\u1EC3 \u0111o\xE1n\
  \ tr\u01B0\u1EDBc \u0111\u01B0\u1EE3c, \u0111\xF3 l\xE0 \u0111i\u1EC1u thi\u1EBF\
  t y\u1EBFu cho c\xE1c \u1EE9ng d\u1EE5ng nh\u01B0 tr\xF2 ch\u01A1i, m\xF4\u2026"
title: "Sinh s\u1ED1 ng\u1EABu nhi\xEAn"
---

{{< edit_this_page >}}

## Mục đích & Lý do?
Việc tạo số ngẫu nhiên trong Elm bao gồm việc tạo ra những giá trị số không thể đoán trước được, đó là điều thiết yếu cho các ứng dụng như trò chơi, mô phỏng và các thuật toán bảo mật. Lập trình viên sử dụng sự ngẫu nhiên để mô phỏng sự khác biệt của thế giới thực, tăng cường trải nghiệm người dùng hoặc bảo vệ dữ liệu với các kỹ thuật mã hóa.

## Cách thực hiện:
Elm xử lý sự ngẫu nhiên một cách khác biệt so với nhiều ngôn ngữ lập trình, sử dụng một hệ thống để giữ cho các hàm được tinh khiết. Để tạo số ngẫu nhiên, bạn phải làm việc với mô-đun `Random` của Elm. Dưới đây là một ví dụ cơ bản về việc tạo một số ngẫu nhiên từ 1 đến 100:

```Elm
import Html exposing (Html, text)
import Random

main : Html msg
main =
    Random.generate NewRandomNumber (Random.int 1 100)
    |> Html.map (text << toString)

type Msg = NewRandomNumber Int
```

Đoạn mã này sử dụng `Random.generate` để tạo một lệnh mà, khi được thực hiện, sẽ sản xuất ra một số ngẫu nhiên trong phạm vi đã chỉ định. Khai báo `type Msg` được sử dụng để xử lý số được tạo trong hàm cập nhật của ứng dụng Elm.

Đối với một ví dụ tương tác hơn, hãy xem xét một tình huống trong đó người dùng kích hoạt việc tạo số ngẫu nhiên thông qua một cú nhấp chuột:

```Elm
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Random

type alias Model = Int

type Msg = Generate

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Generate ->
            (model, Random.generate NewRandomNumber (Random.int 1 100))

view : Model -> Html Msg
view model =
    div []
        [ text ("Generated number: " ++ String.fromInt model)
        , button [ onClick Generate ] [ text "Tạo số mới" ]
        ]

type Msg = NewRandomNumber Int
```

Ứng dụng Elm này đưa vào tính tương tác, cập nhật hiển thị với một số ngẫu nhiên mới mỗi khi người dùng nhấn vào nút.

## Sâu hơn
Thiết kế của hệ thống tạo số ngẫu nhiên của Elm bắt nguồn từ cam kết của ngôn ngữ về sự tinh khiết và tính dự đoán được. Thay vì sử dụng trực tiếp các hàm không tinh khiết mà trả về các giá trị khác nhau mỗi lần gọi, Elm đóng gói sự ngẫu nhiên trong một cấu trúc `Cmd`, phù hợp với kiến trúc của nó là tách biệt các tác dụng phụ khỏi các hàm tinh khiết.

Mặc dù cách tiếp cận này đảm bảo sự nhất quán trong hành vi ứng dụng và tạo điều kiện thuận lợi cho việc gỡ lỗi, nó tạo ra thách thức học hỏi cho những người đã quen với việc tạo số ngẫu nhiên theo phong cách mệnh lệnh. Tuy nhiên, lợi ích của việc duy trì sự tinh khiết của ứng dụng và dễ dàng kiểm tra thường vượt qua sự phức tạp ban đầu.

Phương pháp của Elm cũng tương phản với các ngôn ngữ cung cấp các bộ tạo số ngẫu nhiên toàn cục, có thể dẫn đến các lỗi tinh tế do trạng thái được chia sẻ. Bằng cách yêu cầu xử lý rõ ràng việc tạo số ngẫu nhiên và tác động của nó, Elm khuyến khích các nhà phát triển suy nghĩ một cách critter về nơi và cách thức ngẫu nhiên ảnh hưởng đến ứng dụng của họ, dẫn đến mã đáng tin cậy và có thể dự đoán hơn.

Về các lựa chọn thay thế, các ngôn ngữ chức năng khác cung cấp các chức năng tương tự nhưng có thể triển khai chúng một cách khác biệt. Haskell, ví dụ, cũng duy trì sự tinh khiết trong việc tạo số ngẫu nhiên nhưng thông qua việc sử dụng monads, một khái niệm mà Elm cố tình tránh để đơn giản hoá mô hình của mình. So sánh, cách tiếp cận của Elm dễ tiếp cận hơn đối với người mới và nhấn mạnh một kiến trúc ứng dụng rõ ràng mà không hy sinh sức mạnh của các nguyên tắc lập trình chức năng.
