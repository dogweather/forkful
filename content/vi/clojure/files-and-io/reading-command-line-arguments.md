---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:23.532886-07:00
description: "\u0110\u1ECDc c\xE1c tham s\u1ED1 d\xF2ng l\u1EC7nh cho ph\xE9p m\u1ED9\
  t ch\u01B0\u01A1ng tr\xECnh thu th\u1EADp th\xF4ng tin tr\u1EF1c ti\u1EBFp t\u1EEB\
  \ l\u1EC7nh terminal c\u1EE7a ng\u01B0\u1EDDi d\xF9ng. L\u1EADp tr\xECnh vi\xEA\
  n l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 t\xF9y ch\u1EC9nh\u2026"
lastmod: '2024-03-11T00:14:09.420684-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1ECDc c\xE1c tham s\u1ED1 d\xF2ng l\u1EC7nh cho ph\xE9p m\u1ED9\
  t ch\u01B0\u01A1ng tr\xECnh thu th\u1EADp th\xF4ng tin tr\u1EF1c ti\u1EBFp t\u1EEB\
  \ l\u1EC7nh terminal c\u1EE7a ng\u01B0\u1EDDi d\xF9ng. L\u1EADp tr\xECnh vi\xEA\
  n l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 t\xF9y ch\u1EC9nh\u2026"
title: "\u0110\u1ECDc c\xE1c \u0111\u1ED1i s\u1ED1 d\xF2ng l\u1EC7nh"
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
