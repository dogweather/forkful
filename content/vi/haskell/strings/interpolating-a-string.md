---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:40.571289-07:00
description: "N\u1ED9i suy chu\u1ED7i cho ph\xE9p b\u1EA1n nh\xFAng tr\u1EF1c ti\u1EBF\
  p c\xE1c bi\u1EBFn v\xE0o trong chu\u1ED7i. \u0110i\u1EC1u n\xE0y \u0111\u01B0\u1EE3\
  c th\u1EF1c hi\u1EC7n cho thu\u1EADn ti\u1EC7n v\xE0 d\u1EC5 \u0111\u1ECDc\u2014\
  kh\xF4ng c\u1EA7n d\u1EA5u c\u1ED9ng hay g\u1ECDi h\xE0m \u0111\u1EC3\u2026"
lastmod: '2024-03-11T00:14:09.972527-06:00'
model: gpt-4-0125-preview
summary: "N\u1ED9i suy chu\u1ED7i cho ph\xE9p b\u1EA1n nh\xFAng tr\u1EF1c ti\u1EBF\
  p c\xE1c bi\u1EBFn v\xE0o trong chu\u1ED7i. \u0110i\u1EC1u n\xE0y \u0111\u01B0\u1EE3\
  c th\u1EF1c hi\u1EC7n cho thu\u1EADn ti\u1EC7n v\xE0 d\u1EC5 \u0111\u1ECDc\u2014\
  kh\xF4ng c\u1EA7n d\u1EA5u c\u1ED9ng hay g\u1ECDi h\xE0m \u0111\u1EC3\u2026"
title: "N\u1ED9i suy chu\u1ED7i k\xFD t\u1EF1"
---

{{< edit_this_page >}}

## Cái này & Tại sao?
Nội suy chuỗi cho phép bạn nhúng trực tiếp các biến vào trong chuỗi. Điều này được thực hiện cho thuận tiện và dễ đọc—không cần dấu cộng hay gọi hàm để xây dựng thông điệp của bạn.

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
