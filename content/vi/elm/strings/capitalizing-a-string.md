---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:36.114907-07:00
description: "Vi\u1EC7c vi\u1EBFt hoa m\u1ED9t chu\u1ED7i ngh\u0129a l\xE0 chuy\u1EC3\
  n k\xFD t\u1EF1 \u0111\u1EA7u ti\xEAn sang ch\u1EEF hoa v\xE0 gi\u1EEF nguy\xEA\
  n ph\u1EA7n c\xF2n l\u1EA1i \u1EDF d\u1EA1ng ch\u1EEF th\u01B0\u1EDDng. L\u1EAD\
  p tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n \u0111i\u1EC1u n\xE0y \u0111\u1ED1i\u2026"
lastmod: '2024-03-11T00:14:09.789347-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c vi\u1EBFt hoa m\u1ED9t chu\u1ED7i ngh\u0129a l\xE0 chuy\u1EC3\
  n k\xFD t\u1EF1 \u0111\u1EA7u ti\xEAn sang ch\u1EEF hoa v\xE0 gi\u1EEF nguy\xEA\
  n ph\u1EA7n c\xF2n l\u1EA1i \u1EDF d\u1EA1ng ch\u1EEF th\u01B0\u1EDDng. L\u1EAD\
  p tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n \u0111i\u1EC1u n\xE0y \u0111\u1ED1i\u2026"
title: "Vi\u1EBFt hoa m\u1ED9t chu\u1ED7i"
---

{{< edit_this_page >}}

## Gì và Tại sao?
Việc viết hoa một chuỗi nghĩa là chuyển ký tự đầu tiên sang chữ hoa và giữ nguyên phần còn lại ở dạng chữ thường. Lập trình viên thực hiện điều này đối với các danh từ riêng, tiêu đề, hoặc để áp dụng một phong cách nhất quán cho các tiêu đề và nội dung văn bản.

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
