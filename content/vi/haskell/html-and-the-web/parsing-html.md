---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:06.053813-07:00
description: "L\xE0m Th\u1EBF N\xE0o: H\xE3y b\u1EAFt tay v\xE0o m\u1ED9t s\u1ED1\
  \ m\xE3 l\u1EC7nh, s\u1EED d\u1EE5ng th\u01B0 vi\u1EC7n `tagsoup` \u0111\u1EC3 ph\xE2\
  n t\xEDch m\u1ED9t \u0111o\u1EA1n HTML \u0111\u01A1n gi\u1EA3n. \u0110\u1EA7u ti\xEA\
  n, h\xE3y ch\u1EAFc ch\u1EAFn c\xE0i \u0111\u1EB7t g\xF3i t\u1EEB\u2026"
lastmod: '2024-03-13T22:44:36.709667-06:00'
model: gpt-4-0125-preview
summary: "H\xE3y b\u1EAFt tay v\xE0o m\u1ED9t s\u1ED1 m\xE3 l\u1EC7nh, s\u1EED d\u1EE5\
  ng th\u01B0 vi\u1EC7n `tagsoup` \u0111\u1EC3 ph\xE2n t\xEDch m\u1ED9t \u0111o\u1EA1\
  n HTML \u0111\u01A1n gi\u1EA3n."
title: "Ph\xE2n T\xEDch C\xFA Ph\xE1p HTML"
weight: 43
---

## Làm Thế Nào:
Hãy bắt tay vào một số mã lệnh, sử dụng thư viện `tagsoup` để phân tích một đoạn HTML đơn giản. Đầu tiên, hãy chắc chắn cài đặt gói từ Hackage qua `cabal install tagsoup`.

```Haskell
import Text.HTML.TagSoup

-- Hãy phân tích một đoạn HTML đơn giản
let html = "<html><body><p>Hello, Haskell!</p></body></html>"

-- Phân tích cú pháp nó
let parsedHtml = parseTags html

-- Tìm các đoạn văn
let paragraphs = partitions (~== "<p>") parsedHtml

-- Lấy văn bản từ đoạn đầu tiên
let firstParagraphText = innerText $ head paragraphs

-- Vào đây!
print firstParagraphText
```

Kết quả mẫu:
```
"Hello, Haskell!"
```
Đoạn mã này phân tích một chuỗi HTML, tìm kiếm các thẻ đoạn văn và in ra văn bản chứa trong đoạn đầu tiên. Gọn gàng và ngọt ngào.

## Sâu Hơn
Việc phân tích cú pháp HTML trong Haskell chưa bao giờ trở nên dễ dàng như ngày nay. Có một thời, mọi người tự làm trình phân tích cú pháp của riêng họ hoặc đấu tranh với các thư viện cấp thấp hơn, phân tích HTML như thể đó là miền Tây hoang dã.

Ngày nay, bạn có nhiều lựa chọn. `tagsoup`, như chúng tôi đã sử dụng, tuyệt vời cho khi cấu trúc HTML được xem xét nhiều hơn là một quy tắc—nó khoan dung với HTML hỗn loạn của thực tế. Nếu bạn đang tìm kiếm sự ngặt nghèo hơn, `html-conduit` kết hợp với `xml-conduit` từ gói `conduit` có thể là sự lựa chọn dành cho bạn. Chúng sử dụng phương pháp truyền dữ liệu và khắt khe hơn về cấu trúc.

Bên trong, các thư viện này chuyển đổi HTML thành một cây hoặc một bữa súp thẻ. Chúng cung cấp các hàm tiện ích để truy vấn và thao tác dữ liệu này, làm cho việc phân tích cú pháp HTML ít đau đầu hơn. Hãy nghĩ về chúng như một bản đồ tìm kho báu, nơi X đánh dấu thẻ đoạn văn.

## Xem Thêm
- [`tagsoup` trên Hackage](https://hackage.haskell.org/package/tagsoup)
- [`html-conduit` trên Hackage](https://hackage.haskell.org/package/html-conduit)
- [Tài liệu về Beautiful Soup](https://www.crummy.com/software/BeautifulSoup/) - Mặc dù không phải Haskell, nhưng cách tiếp cận 'bữa súp thẻ' của Beautiful Soup đã ảnh hưởng lên các thư viện tương tự trong thế giới Haskell.
- [Hàm và Toán tử XPath và XQuery trên W3C](https://www.w3.org/TR/xpath-functions/) - Việc đi sâu vào các tiêu chuẩn có thể thông tin về cấu trúc và truy vấn của các tài liệu XML/HTML, hữu ích cho việc hiểu các chiến lược phân tích cú pháp phía sau.
