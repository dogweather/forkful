---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:33.056059-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Haskell cung c\u1EA5p c\xE1c th\u01B0 vi\u1EC7\
  n nh\u01B0 `xml-conduit` \u0111\u1EC3 x\u1EED l\xFD v\u1EDBi XML. V\xED d\u1EE5\
  \ sau \u0111\xE2y minh h\u1ECDa c\xE1ch ph\xE2n t\xEDch c\xFA ph\xE1p chu\u1ED7\
  i XML v\xE0 truy v\u1EA5n c\xE1c\u2026"
lastmod: '2024-03-13T22:44:36.743040-06:00'
model: gpt-4-0125-preview
summary: "Haskell cung c\u1EA5p c\xE1c th\u01B0 vi\u1EC7n nh\u01B0 `xml-conduit` \u0111\
  \u1EC3 x\u1EED l\xFD v\u1EDBi XML."
title: "L\xE0m vi\u1EC7c v\u1EDBi XML"
weight: 40
---

## Cách thực hiện:
Haskell cung cấp các thư viện như `xml-conduit` để xử lý với XML. Ví dụ sau đây minh họa cách phân tích cú pháp chuỗi XML và truy vấn các phần tử:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Text.XML
import Text.XML.Cursor

main :: IO ()
main = do
  let xmlContent = "<greetings><hello>World!</hello></greetings>"
  let tài liệu = parseLBS_ def $ T.encodeUtf8 $ T.pack xmlContent
  let con trỏ = fromDocument tài liệu

  let các văn bản chào = con trỏ $// element "hello" &/ content
  print các văn bản chào  -- ['World!']
```

Kết quả mẫu:

```
["World!"]
```

## Tìm hiểu sâu hơn
XML, viết tắt của eXtensible Markup Language, đã là một phần quan trọng trong việc truyền dữ liệu dài trước khi sự nổi lên của JSON. Nó mô tả chi tiết, nhưng cứng nhắc và chuẩn hóa, làm cho nó phù hợp với môi trường doanh nghiệp nghiêm ngặt, hệ thống lâu đời và các ngành như tài chính và chăm sóc sức khỏe.

Haskell có một số thư viện cho XML; tuy nhiên, `xml-conduit` là một trong những thư viện mạnh mẽ và được sử dụng rộng rãi nhất do khả năng phân tích cú pháp và phát trực tuyến hiệu quả của nó, là một phần của gia đình `conduit` để xử lý dòng dữ liệu.

Các lựa chọn khác bao gồm `HXT` (Haskell XML Toolbox) sử dụng mũi tên cho việc phân tích cú pháp và biến đổi, cung cấp một quan điểm khác cho việc thao tác XML. Mặc dù `HXT` hiện ít phổ biến hơn do đường học tập dốc hơn, nhưng vẫn là một lựa chọn vững chắc cho một số trường hợp sử dụng.

Khi thực hiện xử lý XML trong Haskell, bạn phải chú ý đến mã hóa, vì chuỗi Haskell là Unicode và dữ liệu XML có thể không phải. Ngoài ra, không gian tên XML có thể thêm vào độ phức tạp cho việc phân tích cú pháp.

## Xem thêm:
- Tài liệu gói `xml-conduit`: https://hackage.haskell.org/package/xml-conduit
- Haskell XML Toolbox (HXT): http://hackage.haskell.org/package/hxt
- Sách "Real World Haskell", Chương 16, về xử lý XML: http://book.realworldhaskell.org/read/xml.html
- Wiki Haskell về XML: https://wiki.haskell.org/XML
