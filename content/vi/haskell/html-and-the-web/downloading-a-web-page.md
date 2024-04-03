---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:16.695908-07:00
description: "T\u1EA3i m\u1ED9t trang web c\xF3 ngh\u0129a l\xE0 l\u1EA5y d\u1EEF\
  \ li\u1EC7u c\u1EE7a n\xF3 qua internet; n\xF3 gi\u1ED1ng nh\u01B0 l\u01B0u m\u1ED9\
  t b\u1EA3n sao \u0111\u1EC3 \u0111\u1ECDc ho\u1EB7c x\u1EED l\xFD \u1EDF c\u1EE5\
  c b\u1ED9. C\xE1c l\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n \u0111i\u1EC1u\u2026"
lastmod: '2024-03-13T22:44:36.710928-06:00'
model: gpt-4-0125-preview
summary: "T\u1EA3i m\u1ED9t trang web c\xF3 ngh\u0129a l\xE0 l\u1EA5y d\u1EEF li\u1EC7\
  u c\u1EE7a n\xF3 qua internet; n\xF3 gi\u1ED1ng nh\u01B0 l\u01B0u m\u1ED9t b\u1EA3\
  n sao \u0111\u1EC3 \u0111\u1ECDc ho\u1EB7c x\u1EED l\xFD \u1EDF c\u1EE5c b\u1ED9\
  ."
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
