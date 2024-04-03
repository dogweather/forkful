---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:38.612527-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Elm x\u1EED l\xFD ng\xE0y th\xE1ng v\u1EDB\
  i module `Time`. B\u1EA1n s\u1EBD nh\u1EADn \u0111\u01B0\u1EE3c th\u1EDDi gian hi\u1EC7\
  n t\u1EA1i d\u01B0\u1EDBi d\u1EA1ng d\u1EA5u th\u1EDDi gian POSIX, sau \u0111\xF3\
  \ chuy\u1EC3n \u0111\u1ED5i sang ng\xE0y."
lastmod: '2024-03-13T22:44:36.558535-06:00'
model: gpt-4-0125-preview
summary: "Elm x\u1EED l\xFD ng\xE0y th\xE1ng v\u1EDBi module `Time`."
title: "L\u1EA5y ng\xE0y hi\u1EC7n t\u1EA1i"
weight: 29
---

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
