---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:11.609251-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1\
  ch b\u1EA1n khi\u1EBFn Haskell \u0111\u1ECDc t\u1EC7p v\u0103n b\u1EA3n m\xE0 kh\xF4\
  ng c\u1EA7n ph\u1EA3i v\u1EAFt m\u1ED3 h\xF4i. M\u1EDF tr\xECnh ch\u1EC9nh s\u1EED\
  a y\xEAu th\xEDch c\u1EE7a b\u1EA1n, v\xE0 h\xE3y b\u1EAFt \u0111\u1EA7u vi\u1EBF\
  t\u2026"
lastmod: '2024-03-13T22:44:36.734909-06:00'
model: gpt-4-0125-preview
summary: "D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch b\u1EA1n khi\u1EBFn Haskell \u0111\
  \u1ECDc t\u1EC7p v\u0103n b\u1EA3n m\xE0 kh\xF4ng c\u1EA7n ph\u1EA3i v\u1EAFt m\u1ED3\
  \ h\xF4i."
title: "\u0110\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n"
weight: 22
---

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
