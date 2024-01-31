---
title:                "Tải trang web"
date:                  2024-01-28T21:59:20.860904-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tải trang web"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/clojure/downloading-a-web-page.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Tải một trang web tức là lấy mã HTML từ một URL để chương trình của bạn có thể tương tác với nó. Các lập trình viên làm điều này để cào dữ liệu, tự động hóa tương tác web hoặc kiểm tra trạng thái trang web.

## Làm thế nào:
Trong Clojure, bạn có thể sử dụng `clj-http` để nhanh chóng tải một trang web. Dưới đây là một ví dụ cơ bản:

```Clojure
(require '[clj-http.client :as client])

(defn download-page [url]
  (client/get url))

;; Sử dụng như thế này:
(defn -main []
  (println (download-page "http://example.com")))
```

Nếu bạn thử, bạn sẽ nhận được một bản đồ đầy đặc tả. Phần hấp dẫn nằm ở `:body` và `:status`.

## Sâu hơn
Trong quá khứ, việc tải web là sử dụng 'wget' hoặc 'curl' trên dòng lệnh. Bây giờ, các ngôn ngữ như Clojure tạo ra một lớp trừu tượng với các thư viện. `clj-http` là một trong số các thư viện đó, nó bao bọc các thành phần HttpComponents của Java cho phong cách chức năng của Clojure.

Có lựa chọn khác? Chắc chắn. Bạn có thể sử dụng trực tiếp `java.net.HttpURLConnection` hoặc chọn một thư viện khác như `http-kit` – nhưng `clj-http` rất thoải mái và chứa đựng hầu hết mọi thứ bạn cần ngay lập tức.

Về mặt kỹ thuật, `clj-http` chuyển yêu cầu của bạn thành một thực thể HTTP Java, thực hiện cuộc gọi, và trả lại phản hồi. Phía sau hậu trường, nó đang xử lý việc chuyển hướng, phân tích các tiêu đề, và quản lý phần nội dung phản hồi để bạn có thể tập trung vào dữ liệu của mình, không phải công việc nền.

## Xem thêm
- Kho GitHub clj-http: [https://github.com/dakrone/clj-http](https://github.com/dakrone/clj-http)
- Clojure http-kit cho một cách tiếp cận khác: [http://www.http-kit.org](http://www.http-kit.org)
- Trang chính thức Clojure để biết thêm về ngôn ngữ: [https://clojure.org](https://clojure.org)
