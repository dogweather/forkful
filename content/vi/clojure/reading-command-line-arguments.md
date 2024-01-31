---
title:                "Đọc các đối số dòng lệnh"
date:                  2024-01-28T22:05:23.532886-07:00
model:                 gpt-4-0125-preview
simple_title:         "Đọc các đối số dòng lệnh"

category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/clojure/reading-command-line-arguments.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì và Tại sao?

Đọc các tham số dòng lệnh cho phép một chương trình thu thập thông tin trực tiếp từ lệnh terminal của người dùng. Lập trình viên làm điều này để tùy chỉnh hành vi của chương trình mà không cần thay đổi mã nguồn.

## Làm thế nào:

Trong Clojure, bạn lấy các tham số dòng lệnh với `*command-line-args*`. Dưới đây là một ví dụ đơn giản:

```clojure
;; Giả sử mã này ở trong một file gọi là `echo.clj`

(defn -main [& args]
  (println "Bạn đã nhập:" args))

;; Để chạy: `clojure echo.clj arg1 arg2 arg3`
```

Kết quả mẫu:

```
Bạn đã nhập: (arg1 arg2 arg3)
```

Cần xử lý chúng? Sử dụng các hàm của bộ sưu tập Clojure.

```clojure
(defn -main [& args]
  (let [processed-args (mapv str/upper-case args)]
    (println "Chữ hoa:" processed-args)))

;; Bây giờ, chạy `clojure echo.clj hello world` sẽ xuất ra:
```

Kết quả mẫu:

```
Chữ hoa: ["HELLO" "WORLD"]
```

## Nghiên Cứu Sâu

`*command-line-args*` là một biến trong Clojure, được thiết lập thành một chuỗi các tham số được truyền cho script. Nó đã tồn tại từ những ngày đầu của Clojure, cho thấy Clojure coi các tham số dòng lệnh như là công dân hạng nhất.

Có lựa chọn khác không? Cách thức Java để lấy các tham số dòng lệnh cũng hoạt động trong Clojure, nhờ vào khả năng tương tác giữa các ngôn ngữ. Nhưng điều đó cồng kềnh hơn.

Xét về chi tiết triển khai, khi Clojure khởi động, nó phân tích các tham số và lưu trữ chúng trong `*command-line-args*`. Script của bạn sau đó có thể làm bất cứ điều gì với chúng—phân tích, bỏ qua, chuyển đổi, bạn tên lên.

## Xem Thêm

- Công cụ CLI chính thức của Clojure: https://clojure.org/guides/deps_and_cli
- Clojure từ cơ bản đến nâng cao: Lập trình dòng lệnh: https://aphyr.com/posts/305-clojure-from-the-ground-up-command-line
- ClojureDocs về *command-line-args*: https://clojuredocs.org/clojure.core/*command-line-args*
