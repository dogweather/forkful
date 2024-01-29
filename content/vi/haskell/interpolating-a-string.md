---
title:                "Nội suy chuỗi ký tự"
date:                  2024-01-28T22:02:40.571289-07:00
model:                 gpt-4-0125-preview
simple_title:         "Nội suy chuỗi ký tự"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/haskell/interpolating-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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
