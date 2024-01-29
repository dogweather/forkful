---
title:                "Làm việc với CSV"
date:                  2024-01-28T22:10:21.239377-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với CSV"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/elm/working-with-csv.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Làm việc với CSV (Comma-Separated Values - Giá trị tách biệt bằng dấu phẩy) có nghĩa là đọc và viết dữ liệu trong định dạng văn bản mà mỗi dòng có các giá trị được tách biệt bằng dấu phẩy. Lập trình viên sử dụng CSV bởi vì đó là một định dạng đơn giản được nhiều công cụ và hệ thống hỗ trợ, làm cho nó trở nên tuyệt vời cho việc trao đổi dữ liệu.

## Cách thức:

Elm không có trình phân tích CSV (parser) tích hợp sẵn, nhưng bạn có thể dễ dàng thêm một trình phân tích bằng cách sử dụng một gói như `elm-csv`. Dưới đây là một ví dụ nhanh về việc phân tích dữ liệu CSV:

```Elm
import Csv

csvData: String
csvData =
    "name,age\nAlice,30\nBob,25"

parseCsv: String -> Result Csv.Error (List (List String))
parseCsv data =
    Csv.decode data

main =
    case parseCsv csvData of
        Ok rows ->
            -- làm gì đó với các hàng
            text (String.join "," (List.head rows |> Maybe.withDefault []))
            
        Err error ->
            -- xử lý lỗi
            text (Csv.Error.toString error)
```

Kết quả mẫu cho trường hợp thành công, hiển thị tiêu đề:

```
name,age
```

## Sâu hơn

CSV đã tồn tại từ đầu những năm 1970; nó đơn giản đến mức trước cả các tiêu chuẩn thực sự. Các lựa chọn khác bao gồm JSON và XML, nhưng CSV vẫn được ưu tiên khi xử lý dữ liệu dạng bảng nặng về số và ít cấu trúc. Trong Elm, bởi vì đó là một ngôn ngữ ở phía front-end, bạn sẽ làm việc bằng cách nhận CSV từ một backend hoặc xử lý một tệp cục bộ được người dùng tải lên. Thực hiện điều này đòi hỏi kiến thức về cổng Elm để tương tác JS hoặc gói tệp để tải lên.

## Xem thêm

- Hướng dẫn Elm về tương tác với JavaScript: [Cổng Elm](https://guide.elm-lang.org/interop/ports.html)
