---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:08.968198-07:00
description: "N\u1ED1i chu\u1ED7i c\xF3 ngh\u0129a l\xE0 gh\xE9p ch\xFAng l\u1EA1\
  i v\u1EDBi nhau t\u1EEB \u0111\u1EA7u \u0111\u1EBFn cu\u1ED1i. L\u1EADp tr\xECnh\
  \ vi\xEAn th\u1EF1c hi\u1EC7n \u0111i\u1EC1u n\xE0y khi h\u1ECD c\u1EA7n gh\xE9\
  p c\xE1c \u0111o\u1EA1n v\u0103n b\u1EA3n l\u1EA1i v\u1EDBi nhau \u0111\u1EC3 t\u1EA1\
  o\u2026"
lastmod: 2024-02-19 22:04:55.879940
model: gpt-4-0125-preview
summary: "N\u1ED1i chu\u1ED7i c\xF3 ngh\u0129a l\xE0 gh\xE9p ch\xFAng l\u1EA1i v\u1EDB\
  i nhau t\u1EEB \u0111\u1EA7u \u0111\u1EBFn cu\u1ED1i. L\u1EADp tr\xECnh vi\xEAn\
  \ th\u1EF1c hi\u1EC7n \u0111i\u1EC1u n\xE0y khi h\u1ECD c\u1EA7n gh\xE9p c\xE1c\
  \ \u0111o\u1EA1n v\u0103n b\u1EA3n l\u1EA1i v\u1EDBi nhau \u0111\u1EC3 t\u1EA1o\u2026"
title: "N\u1ED1i chu\u1ED7i k\xFD t\u1EF1"
---

{{< edit_this_page >}}

## Những gì & Tại sao?
Nối chuỗi có nghĩa là ghép chúng lại với nhau từ đầu đến cuối. Lập trình viên thực hiện điều này khi họ cần ghép các đoạn văn bản lại với nhau để tạo thành một chuỗi mới, như khi tạo ra các thông điệp hoặc tạo đường dẫn tệp.

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
