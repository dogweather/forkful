---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:40.571289-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong Haskell, n\u1ED9i suy chu\u1ED7i kh\xF4\
  ng ph\u1EA3i l\xE0 t\xEDnh n\u0103ng s\u1EB5n c\xF3, nh\u01B0ng v\u1EDBi g\xF3i\
  \ `interpolate`, b\u1EA1n c\xF3 th\u1EC3 ti\u1EBFp c\u1EADn r\u1EA5t g\u1EA7n v\u1EDB\
  i n\xF3. \u0110\u1EA7u ti\xEAn, \u0111\u1EA3m\u2026"
lastmod: '2024-03-13T22:44:36.694298-06:00'
model: gpt-4-0125-preview
summary: "Trong Haskell, n\u1ED9i suy chu\u1ED7i kh\xF4ng ph\u1EA3i l\xE0 t\xEDnh\
  \ n\u0103ng s\u1EB5n c\xF3, nh\u01B0ng v\u1EDBi g\xF3i `interpolate`, b\u1EA1n c\xF3\
  \ th\u1EC3 ti\u1EBFp c\u1EADn r\u1EA5t g\u1EA7n v\u1EDBi n\xF3."
title: "N\u1ED9i suy chu\u1ED7i k\xFD t\u1EF1"
weight: 8
---

## Làm thế nào:
Trong Haskell, nội suy chuỗi không phải là tính năng sẵn có, nhưng với gói `interpolate`, bạn có thể tiếp cận rất gần với nó. Đầu tiên, đảm bảo bạn đã có gói này:

```bash
cabal update
cabal install interpolate
```

Bây giờ, viết một số Haskell:

```haskell
{-# LANGUAGE QuasiQuotes #-}
import Data.String.Interpolate (i)

main :: IO ()
main = do
    let name = "world"
    let greeting = [i|Xin chào, #{name}!|]
    putStrLn greeting
```

Chạy nó:

```
Xin chào, thế giới!
```

## Sâu hơn nữa
Trong lịch sử, Haskell không đi kèm với tính năng nội suy chuỗi ngay từ đầu. Đây là tính năng phổ biến hơn ở các ngôn ngữ kịch bản. Nội suy trong Haskell trở nên mượt mà hơn với sự phát triển của quasiquoters, cho phép bạn định nghĩa cú pháp tùy chỉnh của riêng mình—như `i` của chúng tôi dành cho nội suy chuỗi.

Có các phương án khác không? Chắc chắn, sử dụng `printf` từ `Text.Printf`, hoặc nối chuỗi và biến với `++`. Nhưng những phương án này thiếu đi vẻ đẹp và sự đơn giản của nội suy.

Về mặt thực hiện, `interpolate` biến các chuỗi nội suy của bạn thành các chuỗi Haskell thông thường tại thời điểm biên dịch sử dụng Template Haskell, vì vậy không có sự chậm trễ nào khi chạy code của bạn. Nó thông minh và gọn gàng, giống như Haskell.

## Xem thêm
- [Hackage - gói interpolate](https://hackage.haskell.org/package/interpolate)
- [Hackage - mô-đun Text.Printf](https://hackage.haskell.org/package/base/docs/Text-Printf.html)
- [Wiki Haskell - Quasiquotation](https://wiki.haskell.org/Quasiquotation)
- Để kiểm tra tính năng tạo mẫu phong phú, hãy xem [Hackage - mẫu Mustache](https://hackage.haskell.org/package/mustache)
