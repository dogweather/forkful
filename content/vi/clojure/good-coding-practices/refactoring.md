---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:50.395488-07:00
description: "L\xE0m th\u1EBF n\xE0o: T\xE1i c\u1EA5u tr\xFAc trong Clojure\u2014\
  nh\u1EDD v\xE0o c\xFA ph\xE1p s\u1EA1ch s\u1EBD v\xE0 m\xF4 h\xECnh ch\u1EE9c n\u0103\
  ng\u2014c\xF3 th\u1EC3 v\xF4 c\xF9ng \u0111\u01A1n gi\u1EA3n. H\xE3y x\u1EED l\xFD\
  \ m\u1ED9t t\xECnh hu\u1ED1ng ph\u1ED5 bi\u1EBFn: l\u1EB7p qua\u2026"
lastmod: '2024-03-13T22:44:36.164265-06:00'
model: gpt-4-0125-preview
summary: "T\xE1i c\u1EA5u tr\xFAc trong Clojure\u2014nh\u1EDD v\xE0o c\xFA ph\xE1\
  p s\u1EA1ch s\u1EBD v\xE0 m\xF4 h\xECnh ch\u1EE9c n\u0103ng\u2014c\xF3 th\u1EC3\
  \ v\xF4 c\xF9ng \u0111\u01A1n gi\u1EA3n."
title: "T\xE1i c\u1EA5u tr\xFAc m\xE3"
weight: 19
---

## Làm thế nào:
Tái cấu trúc trong Clojure—nhờ vào cú pháp sạch sẽ và mô hình chức năng—có thể vô cùng đơn giản. Hãy xử lý một tình huống phổ biến: lặp qua các bộ sưu tập. Bạn có thể bắt đầu với vòng lặp `for`, như sau:

```clojure
(defn calculate-sum [numbers]
  (reduce + 0 numbers))

(defn old-way []
  (let [nums (range 1 11)]
    (calculate-sum nums)))
```

Gọi `(old-way)` sẽ cho chúng ta 55, tổng từ 1 đến 10. Nhưng, này, chúng ta có thể tái cấu trúc điều này để trở nên hợp với Clojure hơn:

```clojure
(defn new-way []
  (->> (range 1 11)
       (reduce +)))
```

Hàm `(new-way)` tái cấu trúc sử dụng các macro luồng để truyền dải số trực tiếp vào `reduce`, cắt bỏ phần dư thừa.

## Sâu hơn
Nghệ thuật tái cấu trúc có nguồn gốc từ những ngày đầu của sự phát triển phần mềm nhưng thực sự nhận được sự chú ý với cuốn sách quan trọng "Refactoring: Improving the Design of Existing Code" của Martin Fowler được xuất bản vào năm 1999. Trong Clojure, tái cấu trúc thường dựa trên các nguyên tắc lập trình chức năng, ưa chuộng các hàm thuần túy và cấu trúc dữ liệu bất biến.

Các phương pháp thay thế cho việc tái cấu trúc thủ công trong Clojure có thể bao gồm việc sử dụng các công cụ như Cursive, một plugin phổ biến của IntelliJ IDEA, cung cấp các chức năng tái cấu trúc tự động dành riêng cho Clojure. Còn có clj-refactor, một gói Emacs cho Clojure, cung cấp một loạt các chức năng tái cấu trúc.

Một thách thức đặc biệt khi tái cấu trúc trong Clojure là xử lý trạng thái và các tác dụng phụ trong một mô hình chủ yếu là bất biến và không có tác dụng phụ. Sử dụng cẩn thận các atoms, refs, agents và transient là quan trọng trong việc duy trì cả hiệu suất và độ chính xác trong quá trình tái cấu trúc.

## Xem thêm
- Cuốn sách "Refactoring: Improving the Design of Existing Code" của Martin Fowler cho các khái niệm cơ bản.
- [Clojure Docs](https://clojuredocs.org/) cho các ví dụ cụ thể về mã Clojure thông dụng.
- [clj-refactor](https://github.com/clojure-emacs/clj-refactor.el) cho tự động hóa tái cấu trúc trong Emacs.
- [Cursive](https://cursive-ide.com/) cho người dùng IntelliJ cần trợ giúp tự động tái cấu trúc.
- [Tái cấu trúc cùng Rich Hickey](https://www.infoq.com/presentations/Simple-Made-Easy/) - Một bài nói của người tạo ra Clojure mà, mặc dù không về tái cấu trúc per se, cung cấp cái nhìn vào triết lý Clojure có thể hướng dẫn quyết định tái cấu trúc hiệu quả.
