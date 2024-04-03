---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:08.968198-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Haskell l\xE0m cho vi\u1EC7c n\u1ED1i chu\u1ED7\
  i tr\u1EDF n\xEAn kh\xE1 \u0111\u01A1n gi\u1EA3n v\u1EDBi to\xE1n t\u1EED `(++)`."
lastmod: '2024-03-13T22:44:36.701865-06:00'
model: gpt-4-0125-preview
summary: "Haskell l\xE0m cho vi\u1EC7c n\u1ED1i chu\u1ED7i tr\u1EDF n\xEAn kh\xE1\
  \ \u0111\u01A1n gi\u1EA3n v\u1EDBi to\xE1n t\u1EED `(++)`."
title: "N\u1ED1i chu\u1ED7i k\xFD t\u1EF1"
weight: 3
---

## Cách thực hiện:
Haskell làm cho việc nối chuỗi trở nên khá đơn giản với toán tử `(++)`:

```Haskell
main :: IO ()
main = do
  let hello = "Hello"
  let world = "World!"
  
  -- Sử dụng toán tử (++)
  putStrLn $ hello ++ " " ++ world
  
  -- Đầu ra mẫu: "Hello World!"
```

Nhưng tại sao phải dừng lại ở đó? Bạn cũng có `concat` và `intercalate` từ `Data.List` khi danh sách được liên quan đến:

```Haskell
import Data.List (intercalate, concat)

main :: IO ()
main = do
  let wordsList = ["Haskell", "is", "cool"]
  
  -- Nối danh sách các chuỗi
  putStrLn $ concat wordsList
  -- Đầu ra mẫu: "Haskelliscool"

  -- Chèn các chuỗi với một dấu phân cách
  putStrLn $ intercalate " " wordsList
  -- Đầu ra mẫu: "Haskell is cool"
```

## Đào sâu
Ngày xưa, toán tử `++` của Haskell lấy cảm hứng từ các thao tác tương tự trong các ngôn ngữ như ML. Nó là một điều cổ điển, nhưng không phải lúc nào cũng hiệu quả nhất, đặc biệt là đối với các chuỗi lớn hoặc công việc nối chuỗi khổng lồ. Mỗi lần sử dụng `++` tạo ra một danh sách mới, nghĩa là nếu bạn đang làm việc với dữ liệu lớn, bạn có thể cần một cách tiếp cận hiệu quả hơn.

Có các lựa chọn thay thế? Chắc chắn. Kiểu `Builder` từ `Data.Text.Lazy.Builder` có thể được tối ưu hóa tốt hơn cho việc thao tác văn bản lớn. Nó xây dựng văn bản tiết kiệm hơn bằng cách làm việc từng phần, giảm bớt nhu cầu phải liên tục sao chép toàn bộ.

Ví dụ, làm việc với `Builder`:

```Haskell
import Data.Text.Lazy.Builder (Builder, fromString, toLazyText)
import Data.Text.Lazy.IO as T

main :: IO ()
main = do
  let builder1 = fromString "Haskell"
  let builder2 = fromString " "
  let builder3 = fromString "is"
  let builder4 = fromString " "
  let builder5 = fromString "neat!"

  let result = mconcat [builder1, builder2, builder3, builder4, builder5]
  -- Sử dụng mconcat để hợp nhất các Builder

  T.putStrLn $ toLazyText result
  -- Đầu ra mẫu: "Haskell is neat!"
```

Tại sao lại chọn `Builder` hoặc `concat`? Chúng xử lý dữ liệu lớn một cách đơn giản, cho phép bạn kết hợp văn bản mà không gặp vấn đề về hiệu suất.

## Xem thêm
- Wiki Haskell về [Performance/Strings](https://wiki.haskell.org/Performance/Strings) để đào sâu hơn vào các vấn đề về hiệu suất.
-  Tài liệu [gói Data.Text](https://hackage.haskell.org/package/text) để làm việc với văn bản Unicode trong Haskell.
-  Trang web [Ngôn ngữ Haskell](https://www.haskell.org/) để cập nhật với tất cả mọi thứ về Haskell.
