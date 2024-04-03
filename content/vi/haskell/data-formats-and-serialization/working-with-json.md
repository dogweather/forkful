---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:58.293213-07:00
description: "JSON (JavaScript Object Notation) l\xE0 m\u1ED9t \u0111\u1ECBnh d\u1EA1\
  ng d\u1EEF li\u1EC7u d\u1EF1a tr\xEAn v\u0103n b\u1EA3n \u0111\u1EC3 l\u01B0u tr\u1EEF\
  \ v\xE0 truy\u1EC1n t\u1EA3i d\u1EEF li\u1EC7u. L\u1EADp tr\xECnh vi\xEAn s\u1EED\
  \ d\u1EE5ng n\xF3 v\xEC n\xF3 nh\u1EB9, d\u1EC5\u2026"
lastmod: '2024-03-13T22:44:36.739956-06:00'
model: gpt-4-0125-preview
summary: "JSON (JavaScript Object Notation) l\xE0 m\u1ED9t \u0111\u1ECBnh d\u1EA1\
  ng d\u1EEF li\u1EC7u d\u1EF1a tr\xEAn v\u0103n b\u1EA3n \u0111\u1EC3 l\u01B0u tr\u1EEF\
  \ v\xE0 truy\u1EC1n t\u1EA3i d\u1EEF li\u1EC7u."
title: "L\xE0m vi\u1EC7c v\u1EDBi JSON"
weight: 38
---

## Là Gì & Tại Sao?

JSON (JavaScript Object Notation) là một định dạng dữ liệu dựa trên văn bản để lưu trữ và truyền tải dữ liệu. Lập trình viên sử dụng nó vì nó nhẹ, dễ đọc/viết và không phụ thuộc vào ngôn ngữ.

## Làm Thế Nào:

Trong Haskell, chúng ta xử lý JSON sử dụng thư viện `aeson`. Để bắt đầu, bạn nhập khẩu nó và định nghĩa một kiểu dữ liệu tương ứng với cấu trúc JSON mong muốn của bạn.

```Haskell
{-# LANGUAGE DeriveGeneric #-}

import Data.Aeson
import GHC.Generics

-- Giả sử chúng ta có một đối tượng JSON với "name" và "age"

data Person = Person 
  { name :: String
  , age  :: Int
  } deriving (Generic, Show)

instance FromJSON Person
instance ToJSON Person

-- Giải mã chuỗi JSON
main :: IO ()
main = do
  let jsonString = "{\"name\":\"John\", \"age\":30}"
  let maybePerson = decode jsonString :: Maybe Person
  case maybePerson of
    Nothing -> putStrLn "Lỗi khi giải mã JSON."
    Just person -> print person
```

Đầu Ra:
```
Person {name = "John", age = 30}
```

## Nhìn Sâu Hơn

- **Lịch Sử**: Thiết kế của JSON được ảnh hưởng bởi một tập hợp con của cú pháp JavaScript, và nó đầu tiên được chú ý như một phương án đơn giản thay thế cho XML.
- **Các Phương Án Khác**: Mặc dù JSON là vua cho các API web, các phương án thay thế như XML, YAML, hoặc thậm chí là Protocol Buffers có thể được chọn dựa trên ngữ cảnh và yêu cầu.
- **Chi Tiết Triển Khai**: `aeson` sử dụng hệ thống kiểu dữ liệu của Haskell để ghép nối cấu trúc JSON với các kiểu dữ liệu của Haskell. Giải mã được thực hiện qua các lớp kiểu như `FromJSON`, và mã hóa thông qua `ToJSON`.

## Xem Thêm

- Tài liệu gói `aeson`: [https://hackage.haskell.org/package/aeson](https://hackage.haskell.org/package/aeson)
- Các API JSON thực tế để thực hành: [https://jsonplaceholder.typicode.com/](https://jsonplaceholder.typicode.com/)
- Đặc tả JSON: [https://www.json.org/json-en.html](https://www.json.org/json-en.html)
