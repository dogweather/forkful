---
title:                "Tạo một tập tin tạm thời"
date:                  2024-01-28T21:58:32.606922-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tạo một tập tin tạm thời"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/haskell/creating-a-temporary-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?
Tạo một tệp tạm thời có nghĩa là tạo một tệp để sử dụng trong thời gian ngắn, thường là để quản lý dữ liệu trong khi một chương trình đang thực thi. Lập trình viên làm điều này để tránh làm đầy ổ cứng bằng dữ liệu nhất thời và để làm việc với các tệp một cách an toàn mà không gặp phải xung đột hay rò rỉ dữ liệu.

## Làm sao:
Haskell cung cấp gói `temporary`, bao gồm các hàm tiện ích cho các thao tác tệp tạm thời. Dưới đây là một ví dụ nhanh:

```haskell
import System.IO.Temp (withSystemTempFile)
import System.IO (hPutStrLn, hClose)

main :: IO ()
main = withSystemTempFile "mytemp.txt" $ \tempFilePath tempFileHandle -> do
    -- Viết cái gì đó vào tệp tạm
    hPutStrLn tempFileHandle "Xin chào, tệp tạm thời!"
    -- Đóng tệp (cũng xảy ra tự động!)
    hClose tempFileHandle
    putStrLn $ "Một tệp tạm thời đã được tạo tại: " ++ tempFilePath
```

Kết quả mẫu:

```
Một tệp tạm thời đã được tạo tại: /tmp/mytemp.txt123456
```

## Đào Sâu
Trước đây, quản lý tệp tạm thời có thể là một việc đau đầu và rủi ro về tình trạng đua tranh—hai chương trình cố gắng tạo hoặc sử dụng cùng một tệp. Do đó, gói `temporary` của Haskell đã được tạo ra. Nó cung cấp cho bạn các hàm như `withSystemTempFile`, tạo một tệp tạm và tự động loại bỏ nó khi bạn hoàn thành. Cực kỳ tiện lợi để giữ cho các thao tác tệp của bạn gọn gàng và ngăn nắp.

Có những phương án thay thế như sử dụng gói `unix` cho các thao tác tệp tỉ mỉ, nhưng `temporary` tóm tắt đi sự phức tạp. Khi sử dụng `temporary`, tên tệp là duy nhất nhờ các hàm nội bộ. Không hai tệp tạm nào sẽ xung đột, làm cho cuộc sống của bạn dễ dàng hơn một chút.

Phép màu trong cách tiếp cận của Haskell bao gồm bản chất hàm của nó, đảm bảo rằng các tác động phụ, như việc tạo tệp, được xử lý cẩn thận. Nó dựa vào hệ thống kiểu và IO monad để quản lý tài nguyên một cách trách nhiệm.

## Xem Thêm
- [Tài liệu `System.IO.Temp`](https://hackage.haskell.org/package/temporary-1.3/docs/System-IO-Temp.html): Tài liệu chính thức cho các chức năng tệp tạm.
- [Real-World Haskell, Chương 7, I/O](http://book.realworldhaskell.org/read/io.html): Một phần sách giải thích I/O của Haskell, bao gồm cả việc tạo tệp tạm chi tiết hơn.
