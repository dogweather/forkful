---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:22.320565-07:00
description: "Ghi l\u1EA1i (Logging) c\u01A1 b\u1EA3n l\xE0 qu\xE1 tr\xECnh ghi l\u1EA1\
  i c\xE1c s\u1EF1 ki\u1EC7n v\xE0 d\u1EEF li\u1EC7u \u0111\u1EA7u ra t\u1EEB m\u1ED9\
  t ph\u1EA7n m\u1EC1m khi n\xF3 \u0111ang ch\u1EA1y, coi n\xF3 nh\u01B0 nh\u1EAD\
  t k\xFD c\u1EE7a ph\u1EA7n m\u1EC1m. L\u1EADp tr\xECnh\u2026"
lastmod: '2024-03-13T22:44:36.553210-06:00'
model: gpt-4-0125-preview
summary: "Ghi l\u1EA1i (Logging) c\u01A1 b\u1EA3n l\xE0 qu\xE1 tr\xECnh ghi l\u1EA1\
  i c\xE1c s\u1EF1 ki\u1EC7n v\xE0 d\u1EEF li\u1EC7u \u0111\u1EA7u ra t\u1EEB m\u1ED9\
  t ph\u1EA7n m\u1EC1m khi n\xF3 \u0111ang ch\u1EA1y, coi n\xF3 nh\u01B0 nh\u1EAD\
  t k\xFD c\u1EE7a ph\u1EA7n m\u1EC1m. L\u1EADp tr\xECnh\u2026"
title: Ghi log
weight: 17
---

## Gì & Tại Sao?
Ghi lại (Logging) cơ bản là quá trình ghi lại các sự kiện và dữ liệu đầu ra từ một phần mềm khi nó đang chạy, coi nó như nhật ký của phần mềm. Lập trình viên sử dụng ghi lại để theo dõi những gì đang xảy ra bên dưới lớp vỏ - nó cực kỳ quý giá cho việc gỡ lỗi, theo dõi hành vi hệ thống theo thời gian thực và phân tích hoạt động trong quá khứ để tối ưu hóa hiệu suất hoặc kiểm toán.

## Cách thực hiện:
Kiến trúc Elm không hỗ trợ các tác dụng phụ như việc ghi lịch sử ngay từ đầu - bạn xử lý chúng qua các lệnh, là một phần của kiến trúc ứng dụng của bạn. Với mục đích giáo dục, hãy xem bạn có thể mô phỏng việc ghi lịch sử bằng cách gửi thông điệp tới JavaScript qua cổng giao tiếp (ports) như thế nào.

Trước hết, bạn sẽ định nghĩa một mô-đun cổng:

```Elm
port module Logger exposing (..)

-- Định nghĩa một cổng để gửi lịch sử ra JavaScript
port log : String -> Cmd msg
```

Trong `Main.elm` của bạn, bạn sẽ sử dụng cổng `log` để gửi ra một thông điệp lịch sử:

```Elm
import Logger exposing (log)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        AnEvent ->
            -- một số cập nhật cho mô hình của bạn ở đây
            ( updatedModel, log "AnEvent occurred." )

        AnotherEvent ->
            -- các cập nhật mô hình khác ở đây
            ( anotherUpdatedModel, log "AnotherEvent occurred." )
```

Ở phía JavaScript, bạn sẽ đăng ký cổng `log` để xử lý các thông điệp lịch sử đến:

```JavaScript
var app = Elm.Main.init({ /* ... */ });

app.ports.log.subscribe(function(message) {
    console.log(message);
});
```

Đầu ra mẫu trong console JavaScript sẽ là:

```
AnEvent occurred.
AnotherEvent occurred.
```

## Sâu Hơn
Truyền thống, trong các ngôn ngữ như Python hoặc Java, việc ghi lịch sử được thực hiện bằng cách sử dụng một thư viện ghi lịch sử, cung cấp một API đơn giản để ghi các thông điệp lịch sử ở các cấp độ khác nhau như debug, info, warning, error, và critical.

Elm, với sự tập trung vào tính trong sáng và bất biến, không cung cấp việc ghi lịch sử trực tiếp như thế, do mọi loại IO hay tác dụng phụ được quản lý một cách rõ ràng qua kiến trúc Elm.

Khi bạn cần ghi lịch sử đầy đủ tính năng trong Elm, bạn thường dựa vào các công cụ JavaScript bên ngoài. Cổng giao tiếp, như đã được trình bày ở trên, là cây cầu tới các công cụ này. Mô-đun Debug là một lựa chọn khác, nhưng nó được dành cho việc sử dụng trong quá trình phát triển mà thôi, không phải cho việc ghi lịch sử sản xuất.

Bên cạnh cổng giao tiếp, các lập trình viên thường tận dụng các thông điệp từ trình biên dịch Elm và các cơ sở gỡ lỗi thời gian chạy, như `Debug.log`, mà bạn có thể chèn vào mã của mình để theo dõi các giá trị. Nó bao gồm một biểu thức và ghi kết quả ra console như sau:

```Elm
view model =
    Debug.log "Model Debug" model
    -- mã của bạn cho phần view ở đây
```

Tuy nhiên, điều này cũng không dành cho sản xuất. Các công cụ như elm-logger cung cấp một số trừu tượng hóa qua cổng giao tiếp để ghi lịch sử, mặc dù những cái này cũng dành cho quá trình phát triển hơn là sản xuất.

## Xem Thêm
- Cổng giao tiếp Elm: https://guide.elm-lang.org/interop/ports.html
- Elm `Debug`: https://package.elm-lang.org/packages/elm/core/latest/Debug
- Luận bàn về Elm và ghi lịch sử: https://discourse.elm-lang.org/t/elm-and-logging/546
- API Console JavaScript: https://developer.mozilla.org/en-US/docs/Web/API/Console
- Gói elm-logger: https://package.elm-lang.org/packages/arkgil/elm-logger/latest/
