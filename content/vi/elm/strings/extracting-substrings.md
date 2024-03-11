---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:43.182592-07:00
description: "Vi\u1EC7c tr\xEDch xu\u1EA5t c\xE1c chu\u1ED7i con ngh\u0129a l\xE0\
  \ l\u1EA5y c\xE1c ph\u1EA7n c\u1EE5 th\u1EC3 ra kh\u1ECFi m\u1ED9t chu\u1ED7i. C\xE1\
  c l\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n \u0111i\u1EC1u n\xE0y \u0111\u1EC3\
  \ c\xF4 l\u1EADp, thao t\xE1c, ho\u1EB7c ph\xE2n t\xEDch\u2026"
lastmod: '2024-03-11T00:14:09.797872-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c tr\xEDch xu\u1EA5t c\xE1c chu\u1ED7i con ngh\u0129a l\xE0 l\u1EA5\
  y c\xE1c ph\u1EA7n c\u1EE5 th\u1EC3 ra kh\u1ECFi m\u1ED9t chu\u1ED7i. C\xE1c l\u1EAD\
  p tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n \u0111i\u1EC1u n\xE0y \u0111\u1EC3 c\xF4\
  \ l\u1EADp, thao t\xE1c, ho\u1EB7c ph\xE2n t\xEDch\u2026"
title: "Tr\xEDch xu\u1EA5t chu\u1ED7i con"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Việc trích xuất các chuỗi con nghĩa là lấy các phần cụ thể ra khỏi một chuỗi. Các lập trình viên thực hiện điều này để cô lập, thao tác, hoặc phân tích các phần dữ liệu văn bản.

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
