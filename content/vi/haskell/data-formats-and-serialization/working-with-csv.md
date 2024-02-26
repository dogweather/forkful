---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:11.948902-07:00
description: "L\xE0m vi\u1EC7c v\u1EDBi CSV (Gi\xE1 tr\u1ECB T\xE1ch b\u1EB1ng D\u1EA5\
  u ph\u1EA9y) c\xF3 ngh\u0129a l\xE0 \u0111\u1ECDc v\xE0 ghi d\u1EEF li\u1EC7u d\u01B0\
  \u1EDBi d\u1EA1ng b\u1EA3ng. L\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng CSV do\
  \ t\xEDnh \u0111\u01A1n gi\u1EA3n v\xE0 s\u1EF1 h\u1ED7 tr\u1EE3 r\u1ED9ng\u2026"
lastmod: '2024-02-25T18:49:35.085516-07:00'
model: gpt-4-0125-preview
summary: "L\xE0m vi\u1EC7c v\u1EDBi CSV (Gi\xE1 tr\u1ECB T\xE1ch b\u1EB1ng D\u1EA5\
  u ph\u1EA9y) c\xF3 ngh\u0129a l\xE0 \u0111\u1ECDc v\xE0 ghi d\u1EEF li\u1EC7u d\u01B0\
  \u1EDBi d\u1EA1ng b\u1EA3ng. L\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng CSV do\
  \ t\xEDnh \u0111\u01A1n gi\u1EA3n v\xE0 s\u1EF1 h\u1ED7 tr\u1EE3 r\u1ED9ng\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi CSV"
---

{{< edit_this_page >}}

## Gì và Tại sao?
Làm việc với CSV (Giá trị Tách bằng Dấu phẩy) có nghĩa là đọc và ghi dữ liệu dưới dạng bảng. Lập trình viên sử dụng CSV do tính đơn giản và sự hỗ trợ rộng rãi trên các hệ thống cho việc trao đổi dữ liệu.

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
