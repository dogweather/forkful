---
title:                "Làm việc với YAML"
date:                  2024-01-28T22:11:54.369769-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/haskell/working-with-yaml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

YAML (YAML Ai cũng biết không phải là Ngôn ngữ Đánh dấu) là một định dạng hóa dữ liệu thân thiện với con người. Lập trình viên sử dụng nó cho các tệp cấu hình và trao đổi dữ liệu bởi vì tính dễ đọc và đơn giản của nó.

## Làm thế nào:

Để làm việc với YAML trong Haskell, sử dụng gói `yaml`. Đầu tiên, cài đặt nó:

```shell
cabal install yaml
```

Xác định một cấu trúc dữ liệu, sau đó mã hóa và giải mã YAML:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.YAML
import Data.ByteString (ByteString)

-- Xác định cấu trúc dữ liệu
data Person = Person
    { name :: String
    , age  :: Int
    } deriving (Show)

-- Một thể hiện Person mẫu
examplePerson :: Person
examplePerson = Person "Chris" 30

-- Mã hóa (Haskell sang YAML)
yamlEncode :: Person -> ByteString
yamlEncode = encode

-- Giải mã (YAML sang Haskell)
yamlDecode :: ByteString -> Either String Person
yamlDecode = decodeThrow

main :: IO ()
main = do
    -- Mã hóa sang YAML và in kết quả
    putStrLn "YAML được mã hóa:"
    print $ yamlEncode examplePerson
  
    -- Dữ liệu YAML mẫu
    let exampleYAML = "name: Alex\nage: 25\n"
  
    -- Giải mã từ YAML và in kết quả
    putStrLn "Haskell được giải mã:"
    print $ yamlDecode exampleYAML
```

Kết quả mẫu cho việc mã hóa và giải mã:

```plaintext
YAML được mã hóa:
"age: 30\nname: Chris\n"
Haskell được giải mã:
Right (Person {name = "Alex", age = 25})
```

## Đi sâu nghiên cứu

YAML bắt đầu vào năm 2001, nhắm mục tiêu vào việc hóa dữ liệu và khả năng đọc dễ dàng cho con người. Đây là sự lựa chọn phổ biến cho các tệp cấu hình, như Docker Compose và GitHub Workflows. Các lựa chọn thay thế bao gồm JSON và XML, nhưng cú pháp tối giản của YAML thường được ưa thích vì vẻ ngoài gọn gàng. Khi triển khai YAML trong Haskell, điểm then chốt là xác định các cấu trúc dữ liệu ánh xạ với các cặp khóa-giá trị của YAML. Gói `yaml`, được xây dựng dựa trên thư viện libyaml C, cung cấp hiệu suất tốt và tính tương thích.

## Xem thêm

- Trang web chính thức của YAML: [https://yaml.org](https://yaml.org)
- Gói `yaml` trên Hackage: [https://hackage.haskell.org/package/yaml](https://hackage.haskell.org/package/yaml)
- Gói `aeson`, cho JSON trong Haskell có nhiều điểm tương đồng: [https://hackage.haskell.org/package/aeson](https://hackage.haskell.org/package/aeson)
