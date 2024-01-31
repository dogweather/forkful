---
title:                "Đọc một tệp văn bản"
date:                  2024-01-28T22:05:11.609251-07:00
model:                 gpt-4-0125-preview
simple_title:         "Đọc một tệp văn bản"

category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/haskell/reading-a-text-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Đọc một tệp văn bản trong lập trình cho phép mã của bạn hấp thụ dữ liệu, giống như việc đổ cà phê vào não vào buổi sáng. Chúng ta làm điều này để cung cấp thông tin cho chương trình mà nó không có từ trước, như cài đặt, dữ liệu để xử lý, hoặc hướng dẫn để thực hiện.

## Cách thực hiện:
Dưới đây là cách bạn khiến Haskell đọc tệp văn bản mà không cần phải vắt mồ hôi. Mở trình chỉnh sửa yêu thích của bạn, và hãy bắt đầu viết một số mã.

```Haskell
import System.IO

main = do
    -- Mở một tệp ở chế độ đọc
    handle <- openFile "hello.txt" ReadMode
    -- Đọc nội dung của tệp
    content <- hGetContents handle
    -- In nội dung của tệp
    putStrLn content
    -- Đừng quên đóng cánh tay của tệp!
    hClose handle
```

Chạy điều này, và nếu bạn có "hello.txt" với "Hello, World!" bên trong, bạn sẽ nhận được:

```
Hello, World!
```

Dưới đây là một cách ngắn gọn, mạnh mẽ hơn, làm cùng một việc với ít rắc rối hơn:

```Haskell
-- 'readFile' thực hiện mở và đọc lần lượt
main = do
    content <- readFile "hello.txt"
    putStrLn content
```

Kết quả vẫn là,

```
Hello, World!
```

## Đào Sâu

Từ lâu, các chương trình được coi là những sinh vật không xã hội, chủ yếu xử lý dữ liệu do chính chúng tạo ra. Nhưng độ phức tạp tăng lên, và nhu cầu kéo thông tin từ bên ngoài cũng vậy, do đó việc đọc từ các tệp trở thành một nền tảng.

Haskell cung cấp nhiều cách để đọc tệp. Chúng ta có thể làm điều đó theo cách cấp thấp với `openFile`, `hGetContents`, và `hClose` hoặc thực hiện một cách ngầu với `readFile`, mà gói gọn mọi thứ một cách ngăn nắp.

`readFile` là lười biếng – nó đọc nội dung khi cần thiết, điều này tiết kiệm bộ nhớ cho các tệp lớn nhưng có thể gây ra bất ngờ nếu tệp thay đổi giữa chừng. Cách tiếp cận cấp thấp cho phép kiểm soát nhiều hơn, làm cho nó dễ dự đoán hơn nhưng cũng cồng kềnh hơn. Đối với những văn bản khổng lồ, `hGetLine` của Haskell hoặc các thư viện như `conduit` và `pipes` giúp quản lý bộ nhớ và xử lý một cách tinh tế hơn.

Các hành động `IO` chuẩn của Haskell xử lý tệp bằng cách sử dụng các cơ chế của hệ điều hành cơ bản. Các thư viện trừu tượng hóa những điều này thành các hoạt động thân thiện với người dùng hơn nhưng cuối cùng, chúng được xây dựng dựa trên monad `IO` của Haskell, đảm bảo các hành động diễn ra theo đúng thứ tự.

## Xem Thêm

- Để xem tài liệu chính thức của Haskell, hãy kiểm tra [tài liệu nhập và xuất của Haskell](https://www.haskell.org/tutorial/io.html).
- Nếu bạn khát khao học hỏi thêm, hãy thưởng thức một tách tri thức tại [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/input-and-output).
- Làm sâu sắc hơn hiểu biết về quản lý tệp với [Real World Haskell's take on IO](http://book.realworldhaskell.org/read/io.html).
- Khám phá các thư viện phát trực tuyến cho các tệp lớn với [conduit](https://hackage.haskell.org/package/conduit) và [pipes](https://hackage.haskell.org/package/pipes).
