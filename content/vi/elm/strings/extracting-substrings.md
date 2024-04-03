---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:43.182592-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Elm l\xE0m cho \u0111i\u1EC1u n\xE0y tr\u1EDF\
  \ n\xEAn d\u1EC5 d\xE0ng. \u0110\u1EA7u ti\xEAn, ch\xFAng ta s\u1EED d\u1EE5ng `String.slice`."
lastmod: '2024-03-13T22:44:36.529516-06:00'
model: gpt-4-0125-preview
summary: "Elm l\xE0m cho \u0111i\u1EC1u n\xE0y tr\u1EDF n\xEAn d\u1EC5 d\xE0ng."
title: "Tr\xEDch xu\u1EA5t chu\u1ED7i con"
weight: 6
---

## Cách thực hiện:
Elm làm cho điều này trở nên dễ dàng. Đầu tiên, chúng ta sử dụng `String.slice`:

```Elm
import String exposing (slice)

fullText : String
fullText = "Hello, Elm world!"

-- Trích xuất "Elm"
substring : String
substring = slice 7 10 fullText

-- Đầu ra: "Elm"
```

Bây giờ, chúng ta hãy trở nên linh hoạt hơn một chút với `String.left` và `String.right`:

```Elm
import String exposing (left, right)

-- Lấy 5 ký tự đầu tiên
leftString : String
leftString = left 5 fullText

-- Đầu ra: "Hello"

-- Lấy 5 ký tự cuối cùng
rightString : String
rightString = right 5 fullText

-- Đầu ra: "orld!"
```

## Sâu hơn nữa
Về mặt lịch sử, việc trích xuất chuỗi con cũ kỹ như chính bản thân lập trình. Trong Elm, như trong các ngôn ngữ chức năng khác, các hàm thao tác chuỗi là bất biến - chúng trả về chuỗi mới thay vì thay đổi chuỗi gốc.

Có các lựa chọn khác như `String.dropLeft` và `String.dropRight` tồn tại. Chúng cắt bỏ các ký tự ở một phía của chuỗi:

```Elm
import String exposing (dropLeft, dropRight)

-- Bỏ đi 7 ký tự đầu tiên
droppedLeftString : String
droppedLeftString = dropLeft 7 fullText

-- Đầu ra: "Elm world!"

-- Bỏ đi 6 ký tự cuối cùng
droppedRightString : String
droppedRightString = dropRight 6 fullText

-- Đầu ra: "Hello, Elm"
```

Về mặt triển khai, các hàm này được tích hợp sẵn trong thư viện chuẩn của Elm và xử lý Unicode, mặc dù có những xem xét cần thiết với các cặp surrogate và ký tự kết hợp của Unicode.

## Xem thêm
- Tài liệu mô-đun `String` của Elm: https://package.elm-lang.org/packages/elm/core/latest/String
- Hướng dẫn Elm về chuỗi: https://guide.elm-lang.org/strings/
- MDN Web Docs về Unicode: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/charAt
