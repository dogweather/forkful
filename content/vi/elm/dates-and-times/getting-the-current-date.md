---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:38.612527-07:00
description: "Vi\u1EC7c l\u1EA5y ng\xE0y hi\u1EC7n t\u1EA1i trong Elm c\xF3 ngh\u0129\
  a l\xE0 truy xu\u1EA5t ng\xE0y l\u1ECBch hi\u1EC7n t\u1EA1i t\u1EEB h\u1EC7 th\u1ED1\
  ng. Ch\xFAng ta l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 \u0111\xE1nh d\u1EA5u\
  \ th\u1EDDi gian cho c\xE1c s\u1EF1 ki\u1EC7n, l\xEAn\u2026"
lastmod: 2024-02-19 22:04:55.727050
model: gpt-4-0125-preview
summary: "Vi\u1EC7c l\u1EA5y ng\xE0y hi\u1EC7n t\u1EA1i trong Elm c\xF3 ngh\u0129\
  a l\xE0 truy xu\u1EA5t ng\xE0y l\u1ECBch hi\u1EC7n t\u1EA1i t\u1EEB h\u1EC7 th\u1ED1\
  ng. Ch\xFAng ta l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 \u0111\xE1nh d\u1EA5u\
  \ th\u1EDDi gian cho c\xE1c s\u1EF1 ki\u1EC7n, l\xEAn\u2026"
title: "L\u1EA5y ng\xE0y hi\u1EC7n t\u1EA1i"
---

{{< edit_this_page >}}

## Là gì và Tại sao?
Việc lấy ngày hiện tại trong Elm có nghĩa là truy xuất ngày lịch hiện tại từ hệ thống. Chúng ta làm điều này để đánh dấu thời gian cho các sự kiện, lên lịch cho các công việc, hoặc theo dõi khoảng thời gian.

## Cách thực hiện:
Elm xử lý ngày tháng với module `Time`. Bạn sẽ nhận được thời gian hiện tại dưới dạng dấu thời gian POSIX, sau đó chuyển đổi sang ngày.

```Elm
import Browser
import Task
import Time

type Msg = GetCurrentTime Time.Posix

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GetCurrentTime posixTime ->
            let
                -- Chuyển đổi thời gian POSIX thành một bản ghi ngày
                date = Time.toDate posixTime
            in
            -- Cập nhật mô hình của bạn tương ứng ở đây
            ({ model | date = date }, Cmd.none)

-- Để khởi tạo lấy thời gian hiện tại
getCurrentTime : Cmd Msg
getCurrentTime =
    Task.perform GetCurrentTime Time.now

-- Ví dụ đầu ra:
-- date { year = 2023, month = Mar, day = 26 }
```

## Sâu hơn
Trong các ngôn ngữ web cũ, việc lấy ngày là một dòng code đơn giản. Elm khác biệt. Nó làm cho các hiệu ứng phụ như lấy thời gian hiện tại được biểu hiện rõ ràng thông qua Kiến trúc Elm. Điều này khuyến khích sự trong sáng và khả năng bảo trì của mã.

Các giải pháp thay thế bao gồm sử dụng các gói của bên thứ ba hoặc xử lý ngày tháng trong mã máy chủ của bạn và chuyển chúng đến Elm thông qua các cơ sở hoặc cổng.

Về mặt thực hiện, `Time.now` của Elm lấy thời gian dưới dạng dấu thời gian POSIX (mili giây kể từ kỷ nguyên Unix). Điều này không phụ thuộc vào múi giờ và bạn có thể định dạng nó theo nhu cầu sử dụng các chức năng từ module `Time`.

## Xem thêm
- [Tài liệu Elm Time](https://package.elm-lang.org/packages/elm/time/latest/)
- [Hướng dẫn của Elm về lệnh và đăng ký](https://guide.elm-lang.org/effects/)
