---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:36.114907-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong Elm, b\u1EA1n kh\xF4ng c\xF3 s\u1EB5n\
  \ h\xE0m vi\u1EBFt hoa ngay t\u1EEB \u0111\u1EA7u, nh\u01B0ng b\u1EA1n c\xF3 th\u1EC3\
  \ d\u1EC5 d\xE0ng t\u1EA1o m\u1ED9t h\xE0m nh\u01B0 sau."
lastmod: '2024-03-13T22:44:36.521757-06:00'
model: gpt-4-0125-preview
summary: "Trong Elm, b\u1EA1n kh\xF4ng c\xF3 s\u1EB5n h\xE0m vi\u1EBFt hoa ngay t\u1EEB\
  \ \u0111\u1EA7u, nh\u01B0ng b\u1EA1n c\xF3 th\u1EC3 d\u1EC5 d\xE0ng t\u1EA1o m\u1ED9\
  t h\xE0m nh\u01B0 sau."
title: "Vi\u1EBFt hoa m\u1ED9t chu\u1ED7i"
weight: 2
---

## Làm thế nào:
Trong Elm, bạn không có sẵn hàm viết hoa ngay từ đầu, nhưng bạn có thể dễ dàng tạo một hàm như sau:

```Elm
import String exposing (toUpper, toLower, left, dropLeft)

capitalize : String -> String
capitalize text =
    if String.isEmpty text then
        ""
    else
        toUpper (left 1 text) ++ toLower (dropLeft 1 text)

main =
    String.words "hello elm world" |> List.map capitalize |> String.join " "
    -- Kết quả: "Hello Elm World"
```

## Sâu hơn
Elm ưa chuộng sự rõ ràng và không bao gồm các thao tác chuỗi phổ biến như `capitalize` trong thư viện cốt lõi. Theo lịch sử, bạn sẽ tự viết giải pháp của mình hoặc sử dụng thư viện bên thứ ba mở rộng thao tác chuỗi.

Thư viện `String` cốt lõi của Elm cung cấp `toUpper` và `toLower`, xử lý việc chuyển đổi toàn bộ chuỗi. Để viết hoa, bạn lấy ký tự đầu tiên bằng cách sử dụng `left`, viết hoa nó với `toUpper`, sau đó nối nó với phần còn lại của chuỗi, được chuyển thành chữ thường bởi `toLower`. Phần còn lại của chuỗi được trích xuất bằng cách sử dụng `dropLeft`, giúp tránh ảnh hưởng đến ký tự đầu tiên.

Mặc dù thư viện tiêu chuẩn của Elm có thể thiếu một hàm `capitalize` bản địa, quyết định này đảm bảo một hạt nhân tối giản và hiệu suất cao, để vai trò này cho việc triển khai của người dùng hoặc các gói bổ sung.

Các lựa chọn khác bao gồm sử dụng các gói thao tác chuỗi đầy đủ như `elm-string-extra`, bao gồm một hàm `capitalize` cùng với các thao tác chuỗi hữu ích khác:

```Elm
import String.Extra exposing (capitalize)

main =
    String.words "hello elm world" |> List.map capitalize |> String.join " "
    -- Kết quả: "Hello Elm World"
```

Lưu ý rằng cách tiếp cận của Elm đối với chuỗi là nhận biết Unicode, có nghĩa là nó xử lý việc viết hoa chính xác ngay cả đối với các ngôn ngữ có bảng chữ cái không phải Latin, dù có thêm phức tạp.

## Xem thêm
- Tài liệu Elm `String`: https://package.elm-lang.org/packages/elm/core/latest/String
- Thư viện `elm-string-extra` trên Elm packages: https://package.elm-lang.org/packages/elm-community/string-extra/latest/
- Tiêu chuẩn Unicode về việc ánh xạ trường hợp chữ cái: https://www.unicode.org/reports/tr21/
