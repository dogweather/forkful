---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:06.332574-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: B\u1ED9 th\u01B0 vi\u1EC7n phong ph\xFA\
  \ c\u1EE7a Haskell, nh\u01B0 'Data.List', cung c\u1EA5p nhi\u1EC1u c\xF4ng c\u1EE5\
  \ \u0111\u1EC3 thao t\xE1c v\u1EDBi danh s\xE1ch, m\xE0 chu\u1ED7i v\u1EC1 c\u01A1\
  \ b\u1EA3n l\xE0 m\u1ED9t tr\u01B0\u1EDDng h\u1EE3p\u2026"
lastmod: '2024-04-05T21:53:38.078795-06:00'
model: gpt-4-0125-preview
summary: "B\u1ED9 th\u01B0 vi\u1EC7n phong ph\xFA c\u1EE7a Haskell, nh\u01B0 'Data.List',\
  \ cung c\u1EA5p nhi\u1EC1u c\xF4ng c\u1EE5 \u0111\u1EC3 thao t\xE1c v\u1EDBi danh\
  \ s\xE1ch, m\xE0 chu\u1ED7i v\u1EC1 c\u01A1 b\u1EA3n l\xE0 m\u1ED9t tr\u01B0\u1EDD\
  ng h\u1EE3p \u0111\u1EB7c bi\u1EC7t c\u1EE7a."
title: "X\xF3a c\xE1c k\xFD t\u1EF1 ph\xF9 h\u1EE3p v\u1EDBi m\u1ED9t m\u1EABu"
weight: 5
---

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
