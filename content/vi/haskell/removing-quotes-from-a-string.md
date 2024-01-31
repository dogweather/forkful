---
title:                "Loại bỏ dấu ngoặc kép khỏi chuỗi"
date:                  2024-01-28T22:06:43.053660-07:00
model:                 gpt-4-0125-preview
simple_title:         "Loại bỏ dấu ngoặc kép khỏi chuỗi"

category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/haskell/removing-quotes-from-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Lý do & Tại sao?
Xóa dấu ngoặc khỏi một chuỗi có nghĩa là loại bỏ bất kỳ dấu ngoặc đơn (' ') hoặc dấu ngoặc kép (" ") nào là một phần của dữ liệu chuỗi. Lập trình viên thực hiện việc này để làm sạch đầu vào, chuẩn bị văn bản để xử lý, hoặc loại bỏ những ký tự không cần thiết có thể can thiệp vào việc xử lý và thao tác dữ liệu.

## Cách thực hiện:
Trong Haskell, chúng ta có thể tạo ra một hàm xóa tất cả dấu ngoặc khỏi một chuỗi đã cho. Đó giống như việc bảo các dấu ngoặc đi chỗ khác, và đảm bảo rằng chúng nhận ra điều đó.

```Haskell
import Data.List (intercalate)
import Data.Char (isPunctuation)

removeQuotes :: String -> String
removeQuotes = filter (\c -> c /= '"' && c /= '\'')

main :: IO ()
main = do
    let stringWithQuotes = "Haskell said, \"Let's learn some functions!\""
    putStrLn $ removeQuotes stringWithQuotes
```

Kết quả Mẫu:

```
Haskell said, Lets learn some functions!
```

## Sâu hơn nữa
Ngày xưa, trước khi chuỗi trong lập trình trở nên phổ biến như video mèo trên internet, việc xử lý văn bản là một công việc khó khăn. Nhưng khi ngôn ngữ lập trình phát triển, chuỗi trở thành một phần quan trọng trong việc lập trình. Tuy nhiên, dấu ngoặc vẫn là một con dao hai lưỡi - cần thiết cho việc định nghĩa chuỗi, nhưng lại là một rắc rối khi được bao gồm như dữ liệu thực tế.

Có lựa chọn thay thế? Thay vì loại bỏ tất cả các dấu ngoặc như loại bỏ ruồi, bạn có thể chọn lọc. Bạn có thể muốn xoá chỉ các dấu ngoặc ở bên ngoài (một việc cắt tỉa cổ điển) hoặc xử lý các dấu ngoặc được thoát trong chuỗi.

Về mặt triển khai, hàm `removeQuotes` ở trên sử dụng một lambda để kiểm tra từng ký tự (`c`) xem nó có phải là một dấu ngoặc phiền phức không và lọc chúng ra một cách tương ứng. Đây là một cách tiếp cận đơn giản, nhưng đối với các văn bản lớn hơn hoặc quy tắc phức tạp hơn, bạn có thể muốn xem các thư viện phân tích như `Parsec` có thể mang lại cho bạn sự tinh tế và sức mạnh hơn trong xử lý văn bản.

## Xem thêm:
- Dành cho những người yêu thích regex: [Text.Regex.Posix](https://hackage.haskell.org/package/regex-posix)
- Một giới thiệu nhẹ nhàng về chuỗi Haskell: [Learn You a Haskell for Great Good! - Starting Out](http://learnyouahaskell.com/starting-out#strings)
