---
changelog:
- 2024-01-21, dogweather, Reviewed for accuracy
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:50.147581-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: C\xE1c d\u1EF1 \xE1n Haskell th\u01B0\u1EDD\
  ng ph\u1EE5 thu\u1ED9c v\xE0o c\xE1c c\xF4ng c\u1EE5 nh\u01B0 Stack ho\u1EB7c Cabal.\
  \ Stack qu\u1EA3n l\xFD c\xE1c s\u1EF1 ph\u1EE5 thu\u1ED9c, \u0111\u1EA3m b\u1EA3\
  o x\xE2y d\u1EF1ng nh\u1EA5t qu\xE1n. V\xE0o\u2026"
lastmod: '2024-04-05T21:53:38.099392-06:00'
model: gpt-4-0125-preview
summary: "C\xE1c d\u1EF1 \xE1n Haskell th\u01B0\u1EDDng ph\u1EE5 thu\u1ED9c v\xE0\
  o c\xE1c c\xF4ng c\u1EE5 nh\u01B0 Stack ho\u1EB7c Cabal."
title: "B\u1EAFt \u0111\u1EA7u m\u1ED9t d\u1EF1 \xE1n m\u1EDBi"
weight: 1
---

## Cách thực hiện:
```Haskell
-- 1. Khởi tạo một dự án Haskell mới sử dụng Stack
$ stack new myproject

-- Lệnh trên tạo một thư mục mới `myproject` với một số tệp tin:
-- myproject/
-- ├── app/
-- │   └── Main.hs        # Tệp tin ứng dụng chính của bạn
-- ├── src/               # Tệp tin nguồn cho thư viện
-- ├── test/              # Tệp tin kiểm tra
-- ├── myproject.cabal    # Tệp mô tả gói
-- ├── stack.yaml         # Cấu hình Stack
-- └── Setup.hs           # Kịch bản thiết lập xây dựng

-- 2. Xây dựng dự án
$ cd myproject
$ stack build

-- 3. Chạy dự án Haskell mới của bạn
$ stack run

-- Mẫu đầu ra:
someFunc
```

## Sâu hơn
Các dự án Haskell thường phụ thuộc vào các công cụ như Stack hoặc Cabal. Stack quản lý các sự phụ thuộc, đảm bảo xây dựng nhất quán. Vào năm 2008, Stack đã làm thay đổi cuộc chơi cho Haskell, giải quyết những thiếu sót của Cabal với xung đột gói.

Các lựa chọn khác bao gồm sử dụng riêng lẻ Cabal hoặc những công cụ mới hơn như GHCup hoặc Nix cho những bản xây dựng có thể tái tạo. Bạn có thể chọn Cabal vì sự đơn giản hoặc Nix khi công việc của bạn đòi hỏi sự tái tạo, nhưng Stack tạo ra một sự cân bằng hạnh phúc cho nhiều người.

Bên dưới cùng, `stack new` tận dụng một mẫu để khung một dự án. Nó không chỉ bao gồm mã nguồn của bạn mà còn cả cấu hình cho việc xây dựng và các sự phụ thuộc. Tệp `.cabal` rất quan trọng, chứa metadata và hướng dẫn xây dựng.

## Xem thêm
- Tìm hiểu thêm về Stack: [The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/)
- Khám phá Cabal: [The Haskell Cabal](https://www.haskell.org/cabal/users-guide/)
