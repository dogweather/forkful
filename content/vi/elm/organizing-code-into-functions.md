---
title:                "Sắp xếp mã thành các hàm"
date:                  2024-01-28T22:03:38.100081-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sắp xếp mã thành các hàm"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/elm/organizing-code-into-functions.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì và Tại sao?
Đổ tất cả mã của bạn vào một đống lớn? Ý tưởng tồi. Chia nó thành các hàm? Ý tưởng tốt. Việc này giữ cho mã Elm của bạn được sạch sẽ, có thể tái sử dụng và dễ kiểm thử hơn. Bằng cách tổ chức mã của bạn thành các hàm, bạn nhóm các đoạn mã thực hiện các tác vụ cụ thể lại với nhau, làm cho ứng dụng của bạn dễ bảo trì và dễ hiểu hơn.

## Làm thế nào:
Dưới đây là một đoạn mã Elm với một hàm đơn giản để chào mừng người dùng:

```Elm
module Main exposing (..)

import Html exposing (text)

greetUser : String -> String
greetUser userName =
    "Xin chào, " ++ userName ++ "!"

main =
    text (greetUser "Casey")
```

Chạy nó, và bạn sẽ nhận được kết quả: "Xin chào, Casey!"

Bây giờ, giả sử bạn muốn thêm nhiều cá nhân hóa hơn. Trích xuất thêm chức năng!

```Elm
module Main exposing (..)

import Html exposing (text)

greetUser : String -> String -> String
greetUser greeting userName =
    greeting ++ ", " ++ userName ++ "!"

personalGreeting : String -> String
personalGreeting userName =
    greetUser "Howdy" userName

main =
    text (personalGreeting "Casey")
```

Bây giờ, khi bạn chạy nó: "Howdy, Casey!" Ma thuật? Không, chỉ là các hàm đang làm việc của chúng.

## Sâu hơn
Ngày xửa ngày xưa, mã thường là một chuỗi dài các hướng dẫn (nghĩ về mã spaghetti). Nó là một cơn ác mộng để bảo trì. Sau đó, lập trình cấu trúc đến, và cùng với đó, là các hàm. Elm, như những người tiền nhiệm về lập trình chức năng của mình, phụ thuộc nặng nề vào hàm để tổ chức.

Bạn có thể lồng các hàm, tạo ra các closures, hoặc giữ chúng đơn giản với tính pure. Elm khuyến khích cái sau: các hàm pure với đầu vào và đầu ra được định nghĩa rõ ràng, dẫn đến việc gỡ lỗi và kiểm thử dễ dàng hơn.

Các hàm Elm cũng có thể là higher-order, nghĩa là chúng có thể nhận hoặc trả về các hàm khác. Điều này mở ra một thế giới về khả năng kết hợp. Tuy nhiên, không giống như một số ngôn ngữ khác, Elm không có chức năng quá tải; mỗi hàm phải có một tên duy nhất.

Ngoài ra, Elm áp dụng một hệ thống kiểu đặc biệt mạnh mẽ mà không chỉ kiểm tra các kiểu mà còn suy luận chúng, giảm thiểu mã lặp lại.

Khi so sánh với các lựa chọn khác như tổ chức mã theo cách thủ tục hoặc hướng đối tượng trong các ngôn ngữ khác, cách tiếp cận của Elm nhấn mạnh vào sự đơn giản và dự đoán được. Elm không có các đối tượng hay lớp. Bạn tổ chức mã bằng cách sử dụng các hàm và mô-đun thay vì các lớp và thể hiện.

## Xem thêm
Để tìm hiểu sâu hơn, hãy xem các nguồn lực này:
- Hướng dẫn chính thức về các hàm của Elm: https://guide.elm-lang.org/core_language.html
- Tài liệu gói Elm cho các ví dụ về hàm phức tạp hơn: https://package.elm-lang.org/
- Tìm hiểu về hệ thống kiểu của Elm, hoạt động tốt với việc tổ chức hàm: https://elm-lang.org/docs/types
