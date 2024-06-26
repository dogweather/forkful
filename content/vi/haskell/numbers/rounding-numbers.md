---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:02.206298-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Haskell s\u1EED d\u1EE5ng c\xE1c h\xE0\
  m `round`, `ceiling`, `floor` v\xE0 `truncate` t\u1EEB `Prelude` \u0111\u1EC3 th\u1EF1\
  c hi\u1EC7n l\xE0m tr\xF2n."
lastmod: '2024-03-13T22:44:36.705811-06:00'
model: gpt-4-0125-preview
summary: "Haskell s\u1EED d\u1EE5ng c\xE1c h\xE0m `round`, `ceiling`, `floor` v\xE0\
  \ `truncate` t\u1EEB `Prelude` \u0111\u1EC3 th\u1EF1c hi\u1EC7n l\xE0m tr\xF2n."
title: "L\xE0m tr\xF2n s\u1ED1"
weight: 13
---

## Cách thực hiện:
Haskell sử dụng các hàm `round`, `ceiling`, `floor` và `truncate` từ `Prelude` để thực hiện làm tròn.

```haskell
import Prelude

main :: IO ()
main = do
  let num = 3.567
  print $ round num    -- 4
  print $ ceiling num  -- 4
  print $ floor num    -- 3
  print $ truncate num -- 3
  
  -- Làm tròn đến một chỗ thập phân cụ thể không có trong Prelude.
  -- Đây là một hàm tự tạo:
  let roundTo n f = (fromInteger $ round $ f * (10^n)) / (10.0^^n)
  print $ roundTo 1 num -- 3.6
```

## Tìm hiểu sâu
Trong lịch sử, làm tròn số có ý nghĩa quan trọng trong phân tích số và khoa học máy tính vì nó là chìa khóa để giảm thiểu sự tích lũy sai số trong các tính toán, đặc biệt là trước khi biểu diễn số dấu phẩy động được tiêu chuẩn hóa với IEEE 754.

Nên làm tròn đến đâu? `round` đưa bạn đến số nguyên gần nhất—lên hoặc xuống. `ceiling` và `floor` luôn làm tròn lên hoặc xuống đến số nguyên gần nhất, tương ứng, trong khi `truncate` đơn giản là bỏ qua phần thập phân.

Các lựa chọn thay thế cho những hàm này có thể bao gồm lô-gic tùy chỉnh, như `roundTo` của chúng tôi, hoặc bạn có thể tải các thư viện (như Data.Fixed) cho các yêu cầu phức tạp hơn.

Hãy cẩn thận với kết quả không mong đợi do cách Haskell xử lý các trường hợp nửa chừng trong `round` (nó làm tròn đến số chẵn gần nhất).

## Xem thêm
- Tài liệu Haskell Prelude cho các hàm làm tròn: https://hackage.haskell.org/package/base-4.16.1.0/docs/Prelude.html
- Wiki Haskell về toán học số dấu phẩy động: https://wiki.haskell.org/Floating_point_arithmetic
- Tiêu chuẩn IEEE 754-2008 cho thêm thông tin về cách xử lý số dấu phẩy động trong nhiều ngôn ngữ: https://ieeexplore.ieee.org/document/4610935
