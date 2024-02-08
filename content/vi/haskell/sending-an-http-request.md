---
title:                "Gửi một yêu cầu HTTP"
aliases:
- vi/haskell/sending-an-http-request.md
date:                  2024-01-28T22:07:45.267398-07:00
model:                 gpt-4-0125-preview
simple_title:         "Gửi một yêu cầu HTTP"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/haskell/sending-an-http-request.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại Sao?
Gửi một yêu cầu HTTP là hành động yêu cầu một máy chủ web về dữ liệu hoặc hành động. Lập trình viên thực hiện điều này để tương tác với API, lấy nội dung web, hoặc giao tiếp giữa các dịch vụ.

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
