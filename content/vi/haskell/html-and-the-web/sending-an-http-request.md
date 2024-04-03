---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:45.267398-07:00
description: "L\xE0m th\u1EBF n\xE0o: Ch\xFAng ta h\xE3y b\u1EAFt \u0111\u1EA7u v\u1EDB\
  i nh\u1EEFng vi\u1EC7c th\xFA v\u1ECB. B\u1EA1n c\u1EA7n c\xE1c g\xF3i `http-client`\
  \ v\xE0 `http-client-tls`. Thi\u1EBFt l\u1EADp stack c\u1EE7a b\u1EA1n v\xE0 th\xEA\
  m ch\xFAng v\xE0o\u2026"
lastmod: '2024-03-13T22:44:36.708393-06:00'
model: gpt-4-0125-preview
summary: "Ch\xFAng ta h\xE3y b\u1EAFt \u0111\u1EA7u v\u1EDBi nh\u1EEFng vi\u1EC7c\
  \ th\xFA v\u1ECB."
title: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP"
weight: 44
---

## Làm thế nào:
Chúng ta hãy bắt đầu với những việc thú vị. Bạn cần các gói `http-client` và `http-client-tls`. Thiết lập stack của bạn và thêm chúng vào file `package.yaml` hoặc `.cabal`. Sau đó, chạy `stack build` hoặc các lệnh phù hợp khác để tải chúng.

Dưới đây là một yêu cầu GET đơn giản:

```Haskell
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import qualified Data.ByteString.Lazy.Char8 as L8

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings
    request <- parseRequest "http://httpbin.org/get"
    response <- httpLbs request manager
    L8.putStrLn $ responseBody response
```

Điều này sẽ in ra JSON bạn nhận được từ `httpbin.org`.

## Khám Phá Kỹ Lưỡng
Trong quá khứ, việc thực hiện yêu cầu HTTP trong Haskell ít trực tiếp hơn, nhưng những thư viện như `http-client` đã làm đơn giản hóa quá trình này.

Có lựa chọn khác? Chắc chắn rồi. Có `wreq`, `req`, và nhiều người khác, thường với cú pháp đơn giản hoặc tính năng thêm. Nhưng `http-client` giống như cây dao đa năng đáng tin cậy trong ngăn kéo của bạn - nó luôn hoàn thành công việc.

Phần nội dung, `http-client` sử dụng một `Manager` để xử lý các kết nối. Nó hiệu quả và tái sử dụng socket. Bạn có thể điều chỉnh nó, nhưng mặc định là đủ để bắt đầu.

## Xem Thêm
Để mở rộng bộ công cụ của bạn, hãy kiểm tra những cái sau:

- [Gói `http-client`](https://www.stackage.org/package/http-client)
- [Gói `wreq` cho một cách tiếp cận hiện đại hơn](https://www.stackage.org/package/wreq)
- [Hackage cho các thư viện Haskell](https://hackage.haskell.org/)
