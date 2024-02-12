---
title:                "Kiểm tra xem thư mục có tồn tại không"
aliases:
- vi/haskell/checking-if-a-directory-exists.md
date:                  2024-01-28T21:56:12.852469-07:00
model:                 gpt-4-0125-preview
simple_title:         "Kiểm tra xem thư mục có tồn tại không"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/haskell/checking-if-a-directory-exists.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Việc kiểm tra xem một thư mục có tồn tại không có nghĩa là xác nhận liệu thư mục đó có thực sự nằm ở vị trí bạn nghĩ trong hệ thống tệp hay không. Các lập trình viên làm điều này để tránh những lỗi như cố gắng đọc từ một thư mục không tồn tại hoặc vô tình tạo ra các thư mục trùng lặp.

## Làm thế nào:
Haskell sử dụng gói `directory` cho các tương tác hệ thống tệp. Cài đặt nó bằng lệnh `cabal install directory` nếu bạn chưa cài đặt. Dưới đây là cách bạn kiểm tra một thư mục:

```Haskell
import System.Directory (doesDirectoryExist)

main :: IO ()
main = do
    let dir = "path/to/your/directory"
    exists <- doesDirectoryExist dir
    putStrLn $ "Thư mục có tồn tại không? " ++ show exists
```

Nếu `dir` tồn tại, đầu ra của bạn sẽ là:

```
Thư mục có tồn tại không? True
```

Ngược lại, nó sẽ hiển thị:

```
Thư mục có tồn tại không? False
```

## Sâu hơn
Trước đây, bạn có thể đã đối mặt trực tiếp với các lời gọi hệ thống hoặc sử dụng các thư viện ít trừu tượng hơn `directory`. Bây giờ, gói Haskell này thực hiện phần nặng nhọc.

Có phương án thay thế không? Bạn có thể sử dụng các thao tác cấp thấp hơn từ gói `unix`, gọi lệnh shell, hoặc viết các liên kết FFI của riêng bạn. Tất cả đều là quá mức cho một kiểm tra cơ bản như vậy.

Bên dưới, `doesDirectoryExist` sử dụng các lời gọi cụ thể của hệ thống để xác minh sự hiện diện của thư mục mà không gây ra ngoại lệ. Đó là một hành động IO, do đó cần có hàm `main` và `IO ()`.

## Xem Thêm
Các nguồn lực khác để xem xét:

- Tài liệu Haskell địa phương của bạn: `file:///usr/share/doc/ghc/html/libraries/directory/System-Directory.html`
- Hackage cho gói `directory`: [https://hackage.haskell.org/package/directory](https://hackage.haskell.org/package/directory)
