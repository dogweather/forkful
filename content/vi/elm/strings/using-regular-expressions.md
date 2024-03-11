---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:53.415963-07:00
description: "Bi\u1EC3u th\u1EE9c ch\xEDnh quy (regex) l\xE0 c\xE1c m\u1EABu \u0111\
  \u01B0\u1EE3c s\u1EED d\u1EE5ng \u0111\u1EC3 kh\u1EDBp c\xE1c k\u1EBFt h\u1EE3p\
  \ k\xFD t\u1EF1 trong chu\u1ED7i. L\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng ch\xFA\
  ng cho vi\u1EC7c t\xECm ki\u1EBFm, ch\u1EC9nh s\u1EEDa, ho\u1EB7c\u2026"
lastmod: '2024-03-11T00:14:09.799179-06:00'
model: gpt-4-0125-preview
summary: "Bi\u1EC3u th\u1EE9c ch\xEDnh quy (regex) l\xE0 c\xE1c m\u1EABu \u0111\u01B0\
  \u1EE3c s\u1EED d\u1EE5ng \u0111\u1EC3 kh\u1EDBp c\xE1c k\u1EBFt h\u1EE3p k\xFD\
  \ t\u1EF1 trong chu\u1ED7i. L\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng ch\xFAng\
  \ cho vi\u1EC7c t\xECm ki\u1EBFm, ch\u1EC9nh s\u1EEDa, ho\u1EB7c\u2026"
title: "S\u1EED d\u1EE5ng bi\u1EC3u th\u1EE9c ch\xEDnh quy"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Biểu thức chính quy (regex) là các mẫu được sử dụng để khớp các kết hợp ký tự trong chuỗi. Lập trình viên sử dụng chúng cho việc tìm kiếm, chỉnh sửa, hoặc thao tác văn bản, đơn giản hóa các nhiệm vụ như xác nhận biểu mẫu hoặc phân tích dữ liệu.

## Cách thực hiện:

Elm không có khả năng regex tích hợp sẵn, nhưng bạn có thể sử dụng gói `elm/regex`. Dưới đây là cách sử dụng regex cho các nhiệm vụ phổ biến:

```Elm
import Regex exposing (..)

-- Các ví dụ về cách sử dụng regex trong Elm --

-- Kiểm tra xem một chuỗi có chứa "hello" không
checkForHello : String -> Bool
checkForHello input =
    let
        pattern = "hello"
        regex = Regex.fromString pattern |> Maybe.withDefault (regex ".")
    in
    Regex.contains regex input

-- Kết quả mẫu
checkForHello "hello, world!" -- True

-- Trích xuất các chữ số từ một chuỗi
extractDigits : String -> List String
extractDigits input =
    let
        regex = Regex.fromString "\\d+" |> Maybe.withDefault (regex ".")
    in
    Regex.find (All) regex input |> List.map .match

-- Kết quả mẫu
extractDigits "elm123rocks" -- ["123"]
```
Nhớ rằng, bạn cần xử lý Maybe cho các kết quả không khớp mẫu khi sử dụng `Regex.fromString`.

## Sâu hơn nữa

Regex có nguồn gốc từ những năm 1950, với cơ sở trong lý thuyết automata và lý thuyết ngôn ngữ hình thức. Theo thời gian, regex trở thành một công cụ mạnh mẽ trong xử lý văn bản, được tích hợp vào nhiều ngôn ngữ lập trình và các tiện ích dòng lệnh.

Các phương án thay thế cho regex trong Elm bao gồm các hàm chuỗi như `String.contains`, `String.startsWith`, `String.split`, v.v. Mặc dù đơn giản hơn, chúng kém mạnh mẽ hơn cho việc khớp mẫu phức tạp.

Về mặt thực hiện, regex trong Elm được xây dựng trên cơ sở động cơ regex của JavaScript, nhờ vào thời gian chạy của Elm. Điều này có nghĩa là hành vi regex trong Elm có thể phản ánh các khả năng và hạn chế của JavaScript.

## Xem thêm

- Gói Elm Regex: [package.elm-lang.org/packages/elm/regex/latest](https://package.elm-lang.org/packages/elm/regex/latest)
- Biểu thức chính quy trong JavaScript: [developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- Công cụ kiểm tra và gỡ lỗi Regex: [regex101.com](https://regex101.com)
