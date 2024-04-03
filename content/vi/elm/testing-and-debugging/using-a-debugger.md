---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:07.139945-07:00
description: "C\xE1ch th\u1EE9c: Elm kh\xF4ng c\xF3 b\u1ED9 g\u1EE1 l\u1ED7i t\xED\
  ch h\u1EE3p trong b\u1EA3n th\xE2n n\xF3 theo ngh\u0129a truy\u1EC1n th\u1ED1ng\
  \ nh\u01B0, v\xED d\u1EE5, JavaScript v\u1EDBi c\xF4ng c\u1EE5 ph\xE1t tri\u1EC3\
  n tr\xECnh duy\u1EC7t. Tuy nhi\xEAn,\u2026"
lastmod: '2024-03-13T22:44:36.550301-06:00'
model: gpt-4-0125-preview
summary: "Elm kh\xF4ng c\xF3 b\u1ED9 g\u1EE1 l\u1ED7i t\xEDch h\u1EE3p trong b\u1EA3\
  n th\xE2n n\xF3 theo ngh\u0129a truy\u1EC1n th\u1ED1ng nh\u01B0, v\xED d\u1EE5,\
  \ JavaScript v\u1EDBi c\xF4ng c\u1EE5 ph\xE1t tri\u1EC3n tr\xECnh duy\u1EC7t."
title: "S\u1EED d\u1EE5ng b\u1ED9 g\u1EE1 l\u1ED7i"
weight: 35
---

## Cách thức:
Elm không có bộ gỡ lỗi tích hợp trong bản thân nó theo nghĩa truyền thống như, ví dụ, JavaScript với công cụ phát triển trình duyệt. Tuy nhiên, cộng đồng Elm đã xây dựng các công cụ để lấp đầy khoảng trống này. Dưới đây là cách bạn có thể sử dụng `elm-debug-transformer` để gỡ lỗi ứng dụng Elm của bạn:

```Elm
-- Cài đặt elm-debug-transformer (Gói Node)

1. npm install -g elm-debug-transformer

-- Sử dụng elm-debug-transformer để khởi động ứng dụng của bạn

2. elm-debug-transformer --port=8000 yourMainElmFile.elm 
```

Khi `elm-debug-transformer` đang chạy, nó tạo một kết nối WebSocket để ghi nhật ký. Bạn sẽ thấy thông tin gỡ lỗi trong bảng điều khiển của trình duyệt, nơi bạn có thể kiểm tra cấu trúc dữ liệu của chương trình tại các điểm nhất định trong ứng dụng của bạn.

Trong Elm 0.19 và sau đó, các hàm của mô-đun `Debug` như `Debug.log` và `Debug.todo` có thể giúp bạn theo dõi giá trị và đánh dấu một cách cố ý các phần của mã chưa hoàn thiện. Dưới đây là cách sử dụng Debug.log:

```Elm
import Debug

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( Debug.log "Tăng" { model | count = model.count + 1 }, Cmd.none )

        Decrement ->
            ( Debug.log "Giảm" { model | count = model.count - 1 }, Cmd.none )
```

Bạn sẽ thấy thông điệp "Tăng" hoặc "Giảm" trong bảng điều khiển của trình duyệt cùng với trạng thái mới của `model`.

## Sâu hơn
Tác giả của Elm, Evan Czaplicki, đã hướng đến việc tạo ra một ngôn ngữ mà các lỗi phổ biến sẽ không thể xảy ra hoặc dễ dàng bị bắt. Đây là lý do tại sao nhân của Elm không bao gồm các hàm gỡ lỗi truyền thống. Phân tích tĩnh và suy luận kiểu của Elm đóng góp rất lớn trong việc giảm thiểu lỗi thời gian chạy, giảm nhu cầu về gỡ lỗi thời gian chạy phức tạp. Các lựa chọn thay thế trong quá khứ bao gồm sử dụng `elm-reactor` đã lỗi thời, vốn đã cung cấp gỡ lỗi điều hành thời gian - một cách để tua ngược và phát lại các hành động trong ứng dụng của bạn.

Ngày nay, các công cụ như `elm-debug-transformer` và việc sử dụng mô-đun `Debug` của Elm giúp cầu nối khoảng cách. Mặc dù mô-đun `Debug` chỉ dành cho quá trình phát triển và nên được loại bỏ trước khi xây dựng sản xuất, nó là một công cụ không thể thiếu để xác định và ghi lại các thay đổi trạng thái.

Hãy nhớ rằng, các kỹ thuật gỡ lỗi JavaScript truyền thống, như điểm dừng hoặc thực thi từng bước, không trực tiếp áp dụng được trong Elm do kiến trúc của nó và Elm runtime xử lý cập nhật trạng thái. Elm khuyến khích bạn cấu trúc chương trình của mình theo cách dữ liệu dòng chảy rõ ràng và tuân theo các đảm bảo về kiểu và không thay đổi, giảm thiểu trường hợp cần gỡ lỗi.

## Xem thêm
- Hướng dẫn chính thức của Elm về xử lý ngoại lệ thời gian chạy: https://guide.elm-lang.org/error_handling/
- Kho lưu trữ GitHub của `elm-debug-transformer`: https://github.com/kraklin/elm-debug-transformer
- Chủ đề thảo luận của Elm về các chiến lược gỡ lỗi: https://discourse.elm-lang.org/c/show-and-tell/debugging
- Tài liệu mô-đun `Debug` của Elm: https://package.elm-lang.org/packages/elm/core/latest/Debug
