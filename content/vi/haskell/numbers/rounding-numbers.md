---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:02.206298-07:00
description: "L\xE0m tr\xF2n s\u1ED1 c\xF3 ngh\u0129a l\xE0 \u0111i\u1EC1u ch\u1EC9\
  nh ch\xFAng v\u1EC1 g\u1EA7n s\u1ED1 nguy\xEAn ho\u1EB7c ch\u1ED7 th\u1EADp ph\xE2\
  n ch\u1EC9 \u0111\u1ECBnh nh\u1EA5t. L\u1EADp tr\xECnh vi\xEAn l\xE0m tr\xF2n s\u1ED1\
  \ \u0111\u1EC3 ki\u1EC3m so\xE1t \u0111\u1ED9 ch\xEDnh x\xE1c, t\xF9y\u2026"
lastmod: 2024-02-19 22:04:55.884274
model: gpt-4-0125-preview
summary: "L\xE0m tr\xF2n s\u1ED1 c\xF3 ngh\u0129a l\xE0 \u0111i\u1EC1u ch\u1EC9nh\
  \ ch\xFAng v\u1EC1 g\u1EA7n s\u1ED1 nguy\xEAn ho\u1EB7c ch\u1ED7 th\u1EADp ph\xE2\
  n ch\u1EC9 \u0111\u1ECBnh nh\u1EA5t. L\u1EADp tr\xECnh vi\xEAn l\xE0m tr\xF2n s\u1ED1\
  \ \u0111\u1EC3 ki\u1EC3m so\xE1t \u0111\u1ED9 ch\xEDnh x\xE1c, t\xF9y\u2026"
title: "L\xE0m tr\xF2n s\u1ED1"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Làm tròn số có nghĩa là điều chỉnh chúng về gần số nguyên hoặc chỗ thập phân chỉ định nhất. Lập trình viên làm tròn số để kiểm soát độ chính xác, tùy chỉnh kết quả đầu ra cho người dùng hoặc giảm chi phí tính toán cho các thao tác số dấu phẩy động.

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
