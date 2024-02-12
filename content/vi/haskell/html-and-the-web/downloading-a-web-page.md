---
title:                "Tải trang web"
aliases: - /vi/haskell/downloading-a-web-page.md
date:                  2024-01-28T21:59:16.695908-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tải trang web"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/haskell/downloading-a-web-page.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Tải một trang web có nghĩa là lấy dữ liệu của nó qua internet; nó giống như lưu một bản sao để đọc hoặc xử lý ở cục bộ. Các lập trình viên thực hiện điều này để thu thập nội dung, tương tác với các dịch vụ web, hoặc sao chép các trang web.

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
