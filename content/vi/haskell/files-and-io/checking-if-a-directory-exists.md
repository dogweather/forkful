---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:12.852469-07:00
description: "Vi\u1EC7c ki\u1EC3m tra xem m\u1ED9t th\u01B0 m\u1EE5c c\xF3 t\u1ED3\
  n t\u1EA1i kh\xF4ng c\xF3 ngh\u0129a l\xE0 x\xE1c nh\u1EADn li\u1EC7u th\u01B0 m\u1EE5\
  c \u0111\xF3 c\xF3 th\u1EF1c s\u1EF1 n\u1EB1m \u1EDF v\u1ECB tr\xED b\u1EA1n ngh\u0129\
  \ trong h\u1EC7 th\u1ED1ng t\u1EC7p hay kh\xF4ng. C\xE1c l\u1EADp\u2026"
lastmod: '2024-03-13T22:44:36.730999-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c ki\u1EC3m tra xem m\u1ED9t th\u01B0 m\u1EE5c c\xF3 t\u1ED3n t\u1EA1\
  i kh\xF4ng c\xF3 ngh\u0129a l\xE0 x\xE1c nh\u1EADn li\u1EC7u th\u01B0 m\u1EE5c \u0111\
  \xF3 c\xF3 th\u1EF1c s\u1EF1 n\u1EB1m \u1EDF v\u1ECB tr\xED b\u1EA1n ngh\u0129 trong\
  \ h\u1EC7 th\u1ED1ng t\u1EC7p hay kh\xF4ng. C\xE1c l\u1EADp\u2026"
title: "Ki\u1EC3m tra xem th\u01B0 m\u1EE5c c\xF3 t\u1ED3n t\u1EA1i kh\xF4ng"
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
