---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:18.485189-07:00
description: "L\xE0m th\u1EBF n\xE0o: Gi\u1EA3 s\u1EED b\u1EA1n c\xF3 m\u1ED9t h\xE0\
  m Elm \u0111ang l\xE0m qu\xE1 nhi\u1EC1u th\u1EE9, nh\u01B0 l\xE0 m\u1ED7i l\u1EAB\
  n logic UI v\u1EDBi c\u1EADp nh\u1EADt tr\u1EA1ng th\xE1i. \u0110\xF3 l\xE0 \u1EE9\
  ng c\u1EED vi\xEAn ho\xE0n h\u1EA3o cho vi\u1EC7c t\xE1i\u2026"
lastmod: '2024-03-13T22:44:36.555862-06:00'
model: gpt-4-0125-preview
summary: "Gi\u1EA3 s\u1EED b\u1EA1n c\xF3 m\u1ED9t h\xE0m Elm \u0111ang l\xE0m qu\xE1\
  \ nhi\u1EC1u th\u1EE9, nh\u01B0 l\xE0 m\u1ED7i l\u1EABn logic UI v\u1EDBi c\u1EAD\
  p nh\u1EADt tr\u1EA1ng th\xE1i."
title: "T\xE1i c\u1EA5u tr\xFAc m\xE3"
weight: 19
---

## Làm thế nào:
Giả sử bạn có một hàm Elm đang làm quá nhiều thứ, như là mỗi lẫn logic UI với cập nhật trạng thái. Đó là ứng cử viên hoàn hảo cho việc tái cấu trúc. Ban đầu:

```Elm
updateAndFormat : String -> Model -> (Model, Cmd Msg)
updateAndFormat input model =
    let
        updatedModel = { model | userInput = input }
    in
    if String.length input > 5 then
        ( updatedModel, Cmd.none )
    else
        ( model, Cmd.none )
```

Sau khi tái cấu trúc, chúng ta tách riêng mối quan tâm bằng cách đưa logic thành các hàm khác nhau:

```Elm
-- Logic cập nhật là riêng biệt
updateUserInput : String -> Model -> Model
updateUserInput input model = 
    { model | userInput = input }

-- Logic định dạng (view) cũng là riêng biệt
formatUserInput : Model -> (Model, Cmd Msg)
formatUserInput model =
    if String.length model.userInput > 5 then
        ( model, Cmd.none )
    else
        ( { model | userInput = "" }, Cmd.none ) -- Xóa input nếu quá ngắn, như một quy tắc ví dụ.

-- Hàm cập nhật giờ sử dụng các hàm trợ giúp
updateAndFormat : String -> Model -> (Model, Cmd Msg)
updateAndFormat input model =
    model
    |> updateUserInput input
    |> formatUserInput
```
Với những thay đổi này, bạn có sự tách biệt rõ ràng, và mỗi hàm dễ hiểu và kiểm thử hơn.

## Sâu hơn
Tái cấu trúc như một thực hành chính thức có thể truy vết lại đến những ngày đầu của lập trình, khi chi phí của việc thay đổi mã đã được nhận biết là một khía cạnh quan trọng của quy trình phát triển. Đặc biệt, cuốn sách "Refactoring: Improving the Design of Existing Code" của Martin Fowler, xuất bản vào cuối những năm 1990, thực sự đặt nền móng cho việc tái cấu trúc với một cách tiếp cận có cấu trúc và danh mục "mùi mã" để xác định cơ hội tái cấu trúc.

Trong bối cảnh của Elm, tái cấu trúc tận dụng sức mạnh của ngôn ngữ, như hệ thống kiểu mạnh, khuyến khích sự tự tin trong quá trình này. Các phương án thay thế cho việc tái cấu trúc thủ công có thể bao gồm công cụ biến đổi mã tự động, nhưng công cụ của Elm trong khu vực này vẫn đang phát triển so với một số ngôn ngữ cũ hơn. Chi tiết triển khai thường xoay quanh các tái cấu trúc thông thường như trích xuất hàm, đổi tên, và đơn giản hóa điều kiện. Trình biên dịch của Elm là một đồng minh quan trọng trong việc tái cấu trúc, nó sẽ không để bạn làm gì quá mức - nó hét lên mỗi khi có điều gì đó không ổn, đảm bảo mã tái cấu trúc của bạn vẫn hoạt động.

## Xem thêm
- ["Refactoring: Improving the Design of Existing Code" của Martin Fowler](https://martinfowler.com/books/refactoring.html)
- [Diễn đàn Elm - Các chủ đề về Tái cấu trúc](https://discourse.elm-lang.org/search?q=refactoring)
