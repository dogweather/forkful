---
title:                "Bắt đầu một dự án mới"
aliases:
- vi/haskell/starting-a-new-project.md
date:                  2024-01-28T22:08:50.147581-07:00
model:                 gpt-4-0125-preview
simple_title:         "Bắt đầu một dự án mới"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/haskell/starting-a-new-project.md"
changelog:
  - 2024-01-21, dogweather, Reviewed for accuracy
  - 2024-01-28, gpt-4-0125-preview, translated from English
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
