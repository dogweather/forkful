---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:12.852469-07:00
description: "L\xE0m th\u1EBF n\xE0o: Haskell s\u1EED d\u1EE5ng g\xF3i `directory`\
  \ cho c\xE1c t\u01B0\u01A1ng t\xE1c h\u1EC7 th\u1ED1ng t\u1EC7p. C\xE0i \u0111\u1EB7\
  t n\xF3 b\u1EB1ng l\u1EC7nh `cabal install directory` n\u1EBFu b\u1EA1n ch\u01B0\
  a c\xE0i \u0111\u1EB7t. D\u01B0\u1EDBi \u0111\xE2y\u2026"
lastmod: '2024-03-13T22:44:36.730999-06:00'
model: gpt-4-0125-preview
summary: "Haskell s\u1EED d\u1EE5ng g\xF3i `directory` cho c\xE1c t\u01B0\u01A1ng\
  \ t\xE1c h\u1EC7 th\u1ED1ng t\u1EC7p."
title: "Ki\u1EC3m tra xem th\u01B0 m\u1EE5c c\xF3 t\u1ED3n t\u1EA1i kh\xF4ng"
weight: 20
---

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
