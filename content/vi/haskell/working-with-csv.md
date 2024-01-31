---
title:                "Làm việc với CSV"
date:                  2024-01-28T22:10:11.948902-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với CSV"

category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/haskell/working-with-csv.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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
