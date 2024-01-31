---
title:                "Tái cấu trúc mã"
date:                  2024-01-28T22:06:50.395488-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tái cấu trúc mã"

category:             "Clojure"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/clojure/refactoring.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Tái cấu trúc là quá trình cấu trúc lại mã máy tính hiện có mà không thay đổi hành vi bên ngoài của nó, nhằm mục đích cải thiện các thuộc tính phi chức năng. Các lập trình viên tái cấu trúc để mã của họ trở nên sạch sẽ hơn, hiệu quả hơn và dễ bảo trì hơn, từ đó cải thiện tính đọc được và giảm độ phức tạp của phần mềm của họ.

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
