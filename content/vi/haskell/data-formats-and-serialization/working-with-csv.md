---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:11.948902-07:00
description: "L\xE0m th\u1EBF n\xE0o: \u0110\u1EC3 x\u1EED l\xFD CSV trong Haskell,\
  \ b\u1EA1n c\xF3 th\u1EC3 s\u1EED d\u1EE5ng th\u01B0 vi\u1EC7n `cassava`. C\xE0\
  i \u0111\u1EB7t n\xF3 b\u1EB1ng c\xE1ch th\xEAm `cassava` v\xE0o t\u1EC7p `.cabal`\
  \ c\u1EE7a b\u1EA1n ho\u1EB7c s\u1EED d\u1EE5ng\u2026"
lastmod: '2024-03-13T22:44:36.741185-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1EC3 x\u1EED l\xFD CSV trong Haskell, b\u1EA1n c\xF3 th\u1EC3 s\u1EED\
  \ d\u1EE5ng th\u01B0 vi\u1EC7n `cassava`."
title: "L\xE0m vi\u1EC7c v\u1EDBi CSV"
weight: 37
---

## Làm thế nào:
Để xử lý CSV trong Haskell, bạn có thể sử dụng thư viện `cassava`. Cài đặt nó bằng cách thêm `cassava` vào tệp `.cabal` của bạn hoặc sử dụng Stack. Dưới đây là cách giải mã và mã hóa dữ liệu CSV:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

-- Giả sử chúng ta đang làm việc với kiểu này
type Person = (String, Int, Bool)

-- Dữ liệu CSV mẫu
csvData :: BL.ByteString
csvData = "John Doe,30,true\nJane Smith,25,false"

-- Giải mã dữ liệu CSV
decodePeople :: BL.ByteString -> Either String (V.Vector Person)
decodePeople = fmap snd . decode NoHeader

-- Mã hóa dữ liệu thành CSV
encodePeople :: V.Vector Person -> BL.ByteString
encodePeople = encode

-- Ví dụ sử dụng
main :: IO ()
main = do
  -- Giải mã
  case decodePeople csvData of
    Left err -> putStrLn err
    Right v -> print v
  
  -- Mã hóa
  let people = V.fromList [("Alice", 23, True), ("Bob", 35, False)]
  BL.putStrLn $ encodePeople people
```

Đầu ra mẫu:
```plaintext
[("John Doe",30,True),("Jane Smith",25,False)]
"Alice",23,True
"Bob",35,False
```

## Sâu hơn
Việc xử lý CSV trong Haskell đã phát triển. Phương pháp sớm nhất liên quan đến việc phân tích cú pháp chuỗi thủ công, gây ra nhiều lỗi. `cassava` cung cấp phân tích cú pháp kiểu dữ liệu an toàn, dựa vào hệ thống kiểu dữ liệu mạnh mẽ của Haskell. Các lựa chọn khác bao gồm gói `csv`, nhưng `cassava` hiệu quả và linh hoạt hơn. Về mặt thực hiện, `cassava` sử dụng luồng để tăng hiệu quả bộ nhớ và tốc độ, điều này quan trọng khi xử lý các tập dữ liệu lớn.

## Xem thêm
- Thư viện `cassava` trên Hackage: https://hackage.haskell.org/package/cassava
- Thư viện ByteString của Haskell để xử lý dữ liệu nhị phân: https://hackage.haskell.org/package/bytestring
- Hướng dẫn về thư viện Vector, cho danh sách hiệu quả: https://hackage.haskell.org/package/vector
