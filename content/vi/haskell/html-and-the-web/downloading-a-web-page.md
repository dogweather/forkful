---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:16.695908-07:00
description: "L\xE0m th\u1EBF n\xE0o: H\xE3y th\u1EED v\u1EDBi m\u1ED9t v\xED d\u1EE5\
  \ \u0111\u01A1n gi\u1EA3n s\u1EED d\u1EE5ng th\u01B0 vi\u1EC7n `http-conduit` c\u1EE7\
  a Haskell. \u0110\u1EA7u ti\xEAn, c\xE0i \u0111\u1EB7t n\xF3 b\u1EB1ng `cabal install\
  \ http-conduit`. Sau \u0111\xF3."
lastmod: '2024-03-13T22:44:36.710928-06:00'
model: gpt-4-0125-preview
summary: "H\xE3y th\u1EED v\u1EDBi m\u1ED9t v\xED d\u1EE5 \u0111\u01A1n gi\u1EA3n\
  \ s\u1EED d\u1EE5ng th\u01B0 vi\u1EC7n `http-conduit` c\u1EE7a Haskell."
title: "T\u1EA3i trang web"
weight: 42
---

## Làm thế nào:
Hãy thử với một ví dụ đơn giản sử dụng thư viện `http-conduit` của Haskell. Đầu tiên, cài đặt nó bằng `cabal install http-conduit`. Sau đó:

```Haskell
import Network.HTTP.Conduit -- Thư viện mạng chính
import qualified Data.ByteString.Lazy as L -- Chúng ta sẽ cần Lazy ByteStrings

-- Hàm để tải một trang web
downloadPage :: String -> IO L.ByteString
downloadPage url = simpleHttp url

main :: IO ()
main = do
    -- Sử dụng hàm để tải một trang
    noidung <- downloadPage "http://example.com"
    -- Làm gì đó với nội dung, như in ra nó
    L.putStr noidung
```

Chạy chương trình này, bạn sẽ thấy HTML của `http://example.com` trên màn hình của bạn.

## Đi sâu
Các yêu cầu HTTP trong Haskell không luôn luôn gọn gàng như vậy. Các thư viện cũ hơn như `HTTP` yêu cầu nhiều mã dàn trải hơn. Với `http-conduit`, độ phức tạp được trừu tượng hóa đi.

Các phương pháp khác tồn tại, như lệnh `wget` trong một kịch bản shell hay thư viện `requests` của Python. Nhưng những cái này không luôn hiệu quả hoặc biểu đạt tốt trong môi trường chức năng của Haskell.

Bên dưới, `http-conduit` sử dụng một Quản lý để xử lý việc pooling kết nối và Keep-Alive cho HTTP1.1, làm cho nó hiệu quả hơn cho nhiều yêu cầu.

## Xem thêm
- Để sử dụng nâng cao `http-conduit`: [http-conduit trên Hackage](https://hackage.haskell.org/package/http-conduit)
- Để hiểu về ByteString: [ByteString trên Hackage](https://hackage.haskell.org/package/bytestring)
