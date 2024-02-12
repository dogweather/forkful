---
title:                "Viết một tệp văn bản"
aliases:
- /vi/elm/writing-a-text-file/
date:                  2024-01-28T22:13:30.812773-07:00
model:                 gpt-4-0125-preview
simple_title:         "Viết một tệp văn bản"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/elm/writing-a-text-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại Sao?

Việc ghi một tệp văn bản có nghĩa là lưu dữ liệu trong một tệp trên đĩa ở định dạng văn bản. Các lập trình viên thực hiện điều này để lưu trữ dữ liệu, cấu hình, ghi nhật ký, hoặc xuất báo cáo dễ đọc cho con người.

## Làm Thế Nào:

Elm là một ngôn ngữ web ở phía trước, vì vậy nó không thể trực tiếp ghi tệp vào đĩa. Nhưng nó có thể kích hoạt một tải xuống với nội dung mong muốn. Để mô phỏng việc ghi tệp, chúng tôi sẽ tạo một văn bản và sử dụng một liên kết để tải xuống nó dưới dạng một tệp.

```Elm
module Main exposing (main)

import Browser
import Html exposing (Html, a, text, attribute)
import Html.Attributes exposing (href)

createTextFileContent : String
createTextFileContent =
    "Xin chào, Thế giới! Đây là một số nội dung."

createDownloadHref : String -> String
createDownloadHref content =
    "data:text/plain;charset=utf-8," ++ encodeURIComponent(content)

main : Html msg
main =
    a [ href (createDownloadHref createTextFileContent), attribute "download" "myTextFile.txt" ]
        [ text "Tải xuống Tệp Văn bản" ]
```

Kết quả mẫu là một liên kết có thể nhấp vào để tải xuống 'myTextFile.txt' chứa "Xin chào, Thế giới! Đây là một số nội dung."

## Đi Sâu Hơn

Elm chạy trên trình duyệt, vì vậy các hàm cần thiết để trực tiếp ghi vào hệ thống tệp không có sẵn. Lịch sử, JavaScript có những hạn chế tương tự do các hạn chế về bảo mật của trình duyệt. Tuy nhiên, các API web mới hơn và tính năng tương tác với Elm (`Cổng`) cho phép kích hoạt tải xuống hoặc xử lý truy cập hệ thống tệp trong các ứng dụng web. Các phương án khác bao gồm sử dụng ngôn ngữ lập trình phía máy chủ để thao tác tệp trực tiếp hoặc dựa vào các API web như API Truy cập Hệ thống Tệp để mở rộng khả năng trong các trình duyệt hiện đại.

## Xem Thêm

- Hướng dẫn Chính thức của Elm về Tương tác JavaScript (Cổng): [Elm Ports](https://guide.elm-lang.org/interop/ports.html)
- Web API `File` cho xử lý tệp nâng cao trong trình duyệt: [MDN Web Docs - File API](https://developer.mozilla.org/vi/docs/Web/API/File)
- Cái nhìn rộng hơn về kiến trúc Elm: [Kiến Trúc Chính thức của Elm](https://guide.elm-lang.org/architecture/)
