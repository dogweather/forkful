---
aliases:
- /vi/haskell/starting-a-new-project/
changelog:
- 2024-01-21, dogweather, Reviewed for accuracy
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:50.147581-07:00
description: "M\u1ED7i d\u1EF1 \xE1n b\u1EAFt \u0111\u1EA7u v\u1EDBi m\u1ED9t b\u01B0\
  \u1EDBc \u0111i \u0111\u1EA7u ti\xEAn. \u0110\u1ED1i v\u1EDBi c\xE1c l\u1EADp tr\xEC\
  nh vi\xEAn, \u0111i\u1EC1u \u0111\xF3 c\xF3 ngh\u0129a l\xE0 thi\u1EBFt l\u1EAD\
  p c\u1EA5u tr\xFAc ban \u0111\u1EA7u v\xE0 vi\u1EBFt m\xE3 kh\u1EDFi \u0111\u1EA7\
  u. Ch\xFAng ta l\xE0m\u2026"
lastmod: 2024-02-18 23:08:50.745496
model: gpt-4-0125-preview
summary: "M\u1ED7i d\u1EF1 \xE1n b\u1EAFt \u0111\u1EA7u v\u1EDBi m\u1ED9t b\u01B0\u1EDB\
  c \u0111i \u0111\u1EA7u ti\xEAn. \u0110\u1ED1i v\u1EDBi c\xE1c l\u1EADp tr\xECnh\
  \ vi\xEAn, \u0111i\u1EC1u \u0111\xF3 c\xF3 ngh\u0129a l\xE0 thi\u1EBFt l\u1EADp\
  \ c\u1EA5u tr\xFAc ban \u0111\u1EA7u v\xE0 vi\u1EBFt m\xE3 kh\u1EDFi \u0111\u1EA7\
  u. Ch\xFAng ta l\xE0m\u2026"
title: "B\u1EAFt \u0111\u1EA7u m\u1ED9t d\u1EF1 \xE1n m\u1EDBi"
---

{{< edit_this_page >}}

## Gì và Tại sao?
Mỗi dự án bắt đầu với một bước đi đầu tiên. Đối với các lập trình viên, điều đó có nghĩa là thiết lập cấu trúc ban đầu và viết mã khởi đầu. Chúng ta làm điều này để biến ý tưởng thành một nền tảng cụ thể, sẵn sàng cho sự mở rộng và đổi mới.

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
