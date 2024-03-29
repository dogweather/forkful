---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:17.887362-07:00
description: "B\u1EAFt \u0111\u1EA7u m\u1ED9t d\u1EF1 \xE1n m\u1EDBi \u0111\u1ED3\
  ng ngh\u0129a v\u1EDBi vi\u1EC7c thi\u1EBFt l\u1EADp m\u1ED9t m\xF4i tr\u01B0\u1EDD\
  ng l\u1EADp tr\xECnh m\u1EDBi cho \u0111o\u1EA1n m\xE3 c\u1EE7a b\u1EA1n. L\u1EAD\
  p tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 kh\u1EDFi \u0111\u1EA7\
  u c\xF4ng vi\u1EC7c\u2026"
lastmod: '2024-03-13T22:44:36.154087-06:00'
model: gpt-4-0125-preview
summary: "B\u1EAFt \u0111\u1EA7u m\u1ED9t d\u1EF1 \xE1n m\u1EDBi \u0111\u1ED3ng ngh\u0129\
  a v\u1EDBi vi\u1EC7c thi\u1EBFt l\u1EADp m\u1ED9t m\xF4i tr\u01B0\u1EDDng l\u1EAD\
  p tr\xECnh m\u1EDBi cho \u0111o\u1EA1n m\xE3 c\u1EE7a b\u1EA1n. L\u1EADp tr\xEC\
  nh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 kh\u1EDFi \u0111\u1EA7u c\xF4\
  ng vi\u1EC7c\u2026"
title: "B\u1EAFt \u0111\u1EA7u m\u1ED9t d\u1EF1 \xE1n m\u1EDBi"
---

{{< edit_this_page >}}

## Cái gì và Tại sao?

Bắt đầu một dự án mới đồng nghĩa với việc thiết lập một môi trường lập trình mới cho đoạn mã của bạn. Lập trình viên làm điều này để khởi đầu công việc phát triển với một bảng trắng và tổ chức ý tưởng thành mã có thể thực thi.

## Cách thức:

Để khởi tạo một dự án Clojure, chúng ta sẽ sử dụng Leiningen, một công cụ xây dựng phổ biến cho Clojure:

``` Clojure
;; 1. Cài đặt Leiningen nếu bạn chưa có (https://leiningen.org/)
;; 2. Tạo một khung dự án mới:
lein new app my-cool-app

;; 3. Di chuyển vào dự án mới của bạn:
cd my-cool-app

;; 4. Khởi động một REPL (Vòng lặp Đọc-Đánh giá-In):
lein repl

;; Đầu ra mẫu:
;; Máy chủ nREPL đã khởi động tại cổng 12345 trên máy chủ 127.0.0.1 - nrepl://127.0.0.1:12345
;; REPL-y 0.4.4, nREPL 0.6.0
;; Clojure 1.10.1
;; Java 1.8.0_232
;;     Tài liệu: (doc tên-hàm-ở-đây)
;;              (find-doc "phần-của-tên-ở-đây")
;;   Mã nguồn: (source tên-hàm-ở-đây)
;;  Javadoc: (javadoc đối-tượng-hoặc-lớp-java-ở-đây)
;;     Thoát: Control+D hoặc (exit) hoặc (quit)
;;  Kết quả: Lưu trữ trong các biến *1, *2, *3, một ngoại lệ trong *e

;; 5. Tạo một tệp cho mã của bạn (src/my_cool_app/core.clj) và mở nó trong trình soạn thảo văn bản yêu thích của bạn.

;; 6. Viết một số mã Clojure đơn giản:
(ns my-cool-app.core)

(defn say-hello []
  (println "Hello, Clojure world!"))

;; 7. Chạy hàm của bạn trong REPL:
(my-cool-app.core/say-hello)

;; Đầu ra mẫu:
;; Hello, Clojure world!
```

## Sâu hơn

Các dự án Clojure thường bắt đầu với Leiningen hoặc Boot để quản lý phụ thuộc, xây dựng và tự động hóa các nhiệm vụ. Leiningen đã xuất hiện từ năm 2010 và đã trở thành lựa chọn mặc định cho hầu hết những người sử dụng Clojure.

Những công cụ thay thế thì tồn tại, như `deps.edn` và công cụ CLI Clojure, được giới thiệu bởi Clojure/core nhằm cung cấp quản lý phụ thuộc và cấu hình dự án một cách đơn giản hơn.

Chính Clojure đề cao tính bất biến và lập trình hàm. Bắt đầu một dự án đúng cách nhấn mạnh vào quản lý trạng thái sạch sẽ và sự tách biệt mối lo ngại qua các hàm và không gian tên.

Các dự án thường tuân theo cấu trúc thư mục tiêu chuẩn:
- `src/` cho mã chính của bạn.
- `test/` cho mã kiểm tra.
- `resources/` cho nguồn lực không phải mã.
- `project.clj` hoặc `deps.edn` để quản lý phụ thuộc và cấu hình.

Một thói quen tốt là giữ mọi thứ ở mức tối giản từ đầu. Thêm phụ thuộc khi bạn tiến triển, giữ cho dự án của bạn nhẹ nhàng và dễ quản lý.

## Xem thêm

- [Hướng dẫn Bắt đầu với Leiningen](https://leiningen.org/#getting-started)
- [Tài liệu Clojure](https://clojuredocs.org/)
- [Hướng dẫn Cách viết mã Clojure](https://guide.clojure.style/)
- [Công cụ CLI Clojure](https://clojure.org/guides/getting_started)
- [Bộ công cụ Clojure](https://www.clojure-toolbox.com/)
