---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:20.860904-07:00
description: "T\u1EA3i m\u1ED9t trang web t\u1EE9c l\xE0 l\u1EA5y m\xE3 HTML t\u1EEB\
  \ m\u1ED9t URL \u0111\u1EC3 ch\u01B0\u01A1ng tr\xECnh c\u1EE7a b\u1EA1n c\xF3 th\u1EC3\
  \ t\u01B0\u01A1ng t\xE1c v\u1EDBi n\xF3. C\xE1c l\u1EADp tr\xECnh vi\xEAn l\xE0\
  m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 c\xE0o d\u1EEF li\u1EC7u, t\u1EF1 \u0111\u1ED9\
  ng\u2026"
lastmod: '2024-03-13T22:44:36.151439-06:00'
model: gpt-4-0125-preview
summary: "T\u1EA3i m\u1ED9t trang web t\u1EE9c l\xE0 l\u1EA5y m\xE3 HTML t\u1EEB m\u1ED9\
  t URL \u0111\u1EC3 ch\u01B0\u01A1ng tr\xECnh c\u1EE7a b\u1EA1n c\xF3 th\u1EC3 t\u01B0\
  \u01A1ng t\xE1c v\u1EDBi n\xF3."
title: "T\u1EA3i trang web"
weight: 42
---

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
