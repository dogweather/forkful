---
title:                "Xóa các ký tự phù hợp với một mẫu"
aliases: - /vi/haskell/deleting-characters-matching-a-pattern.md
date:                  2024-01-28T21:59:06.332574-07:00
model:                 gpt-4-0125-preview
simple_title:         "Xóa các ký tự phù hợp với một mẫu"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/haskell/deleting-characters-matching-a-pattern.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại Sao?

Xóa các ký tự khớp với một mẫu cụ thể là về việc lọc qua văn bản và loại bỏ các phần bạn không cần. Các lập trình viên làm điều này để làm sạch dữ liệu, đơn giản hóa chuỗi, hoặc chuẩn bị dữ liệu cho điều gì đó quan trọng hơn xuống dòng, như phân tích cú pháp hoặc phân tích.

## Cách thực hiện:

```haskell
import Data.List (isInfixOf)
import Data.Char (isSpace)

-- Hàm đơn giản để xóa một mẫu khỏi chuỗi
removePattern :: Eq a => [a] -> [a] -> [a]
removePattern [] _ = []
removePattern string@(x:xs) pattern
  | pattern `isInfixOf` string = removePattern (drop (length pattern) string) pattern
  | otherwise = x : removePattern xs pattern

-- Sử dụng các hàm đã định trước để cắt bỏ khoảng trắng từ chuỗi
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

main :: IO ()
main = do
  let text = "Haskell thật là tuyệt vời, quả thực rất tuyệt vời."
  let cleanedText = removePattern text "tuyệt vời "
  putStrLn cleanedText  -- "Haskell thật là, quả thực rất."
  putStrLn $ trim "   Khoảng trắng được cắt bỏ   " -- "Khoảng trắng được cắt bỏ"
```

## Sâu hơn

Bộ thư viện phong phú của Haskell, như 'Data.List', cung cấp nhiều công cụ để thao tác với danh sách, mà chuỗi về cơ bản là một trường hợp đặc biệt của. Lịch sử, khớp mẫu của Haskell là một khái niệm được mượn từ các ngôn ngữ hàm cũ hơn như ML.

Có nhiều cách khớp mẫu trong Haskell. Hàm `removePattern` đơn giản của chúng tôi sử dụng `isInfixOf` để kiểm tra mẫu. Cũng có các thư viện biểu thức chính quy cho các mẫu phức tạp, nhưng chúng thêm phụ thuộc và đôi khi làm cho mọi thứ trở nên quá phức tạp.

Nói về phụ thuộc, để cắt bỏ khoảng trắng, bạn có thể nhập một thư viện của bên thứ ba, nhưng hàm 'trim' của chúng tôi đã làm được công việc một cách gốc địa.

Cuối cùng, về mặt hiệu suất, luôn cẩn thận với các hàm đệ quy trong Haskell; chúng có thể không hiệu quả nếu không được trình biên dịch tối ưu hóa đúng cách. Thunks có thể chất chứa, gây rò rỉ không gian. Để tăng hiệu suất, bạn có thể khám phá mô-đun `Text` của Haskell để thao tác với chuỗi lớn hoặc nhiều chuỗi.

## Xem Thêm

- Haskell Thực Tế: http://book.realworldhaskell.org/
- Tài liệu `Data.List` của Haskell: https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-List.html
- Wiki Haskell về Hiệu Suất: https://wiki.haskell.org/Performance
