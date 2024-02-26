---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:11:04.589535-07:00
description: "M\u1EA3ng k\u1EBFt h\u1EE3p, hay b\u1EA3n \u0111\u1ED3 b\u0103m, trong\
  \ Clojure cho ph\xE9p b\u1EA1n l\u01B0u tr\u1EEF v\xE0 truy xu\u1EA5t d\u1EEF li\u1EC7\
  u b\u1EB1ng c\u1EB7p kh\xF3a-gi\xE1 tr\u1ECB. Ch\xFAng l\xE0 gi\u1EA3i ph\xE1p l\xFD\
  \ t\u01B0\u1EDFng \u0111\u1EC3 qu\u1EA3n l\xFD d\u1EEF\u2026"
lastmod: '2024-02-25T18:49:34.519740-07:00'
model: gpt-4-0125-preview
summary: "M\u1EA3ng k\u1EBFt h\u1EE3p, hay b\u1EA3n \u0111\u1ED3 b\u0103m, trong Clojure\
  \ cho ph\xE9p b\u1EA1n l\u01B0u tr\u1EEF v\xE0 truy xu\u1EA5t d\u1EEF li\u1EC7u\
  \ b\u1EB1ng c\u1EB7p kh\xF3a-gi\xE1 tr\u1ECB. Ch\xFAng l\xE0 gi\u1EA3i ph\xE1p l\xFD\
  \ t\u01B0\u1EDFng \u0111\u1EC3 qu\u1EA3n l\xFD d\u1EEF\u2026"
title: "S\u1EED d\u1EE5ng m\u1EA3ng li\xEAn k\u1EBFt"
---

{{< edit_this_page >}}

## Gì và Tại sao?

Mảng kết hợp, hay bản đồ băm, trong Clojure cho phép bạn lưu trữ và truy xuất dữ liệu bằng cặp khóa-giá trị. Chúng là giải pháp lý tưởng để quản lý dữ liệu có cấu trúc, giúp truy cập các phần tử cụ thể nhanh chóng mà không cần phải duyệt qua danh sách.

## Cách thực hiện:

Trong Clojure, việc tạo và thao tác với mảng kết hợp (bản đồ băm) rất đơn giản. Hãy cùng tìm hiểu với các ví dụ.

Để tạo một bản đồ băm:

```clojure
(def my-map {:name "Alex" :age 30})
```

Bạn có thể truy xuất một giá trị bằng cách chỉ định khóa của nó:

```clojure
(get my-map :name)
;; "Alex"
```
Hoặc, một cách phổ biến hơn, bạn có thể sử dụng khóa như một hàm:

```clojure
(:name my-map)
;; "Alex"
```

Thêm hoặc cập nhật các mục là đơn giản:

```clojure
(def updated-map (assoc my-map :location "New York"))
;; {:name "Alex", :age 30, :location "New York"}

(def incremented-age (update my-map :age inc))
;; {:name "Alex", :age 31}
```

Để xóa các khóa, sử dụng `dissoc`:

```clojure
(def removed-age (dissoc my-map :age))
;; {:name "Alex"}
```

Để lặp qua một bản đồ:

```clojure
(doseq [[k v] my-map] (println k "->" v))
;; :name -> Alex
;; :age -> 30
```

Và cho việc truy cập có điều kiện, `find` trả về một cặp khóa-giá trị nếu khóa tồn tại:

```clojure
(find my-map :age)
;; [:age 30]
```

## Tìm hiểu sâu hơn

Mảng kết hợp trong Clojure, còn được gọi là bản đồ băm, là cực kỳ linh hoạt và hiệu quả cho việc quản lý dữ liệu dựa trên cặp khóa-giá trị. Chúng là một phần của thư viện bộ sưu tập phong phú của Clojure, gắn liền với triết lý của ngôn ngữ về tính bất biến và lập trình hàm. Không giống như mảng hay danh sách yêu cầu độ phức tạp thời gian O(n) để truy cập phần tử, bản đồ băm cung cấp độ phức tạp thời gian gần như hằng số để truy cập, làm cho chúng rất hiệu quả cho các thao tác tìm kiếm.

Có thể tranh luận rằng vector trong Clojure có thể phục vụ một mục đích tương tự thông qua truy cập chỉ mục, nhưng bản đồ băm tỏa sáng khi nói đến việc xử lý dữ liệu không tuần tự và có nhãn, nơi khóa cung cấp một mô tả có ý nghĩa hơn là chỉ mục tùy ý.

Đặc biệt đối với Clojure (và di sản Lisp của nó), mảng kết hợp là công dân hạng nhất, có nghĩa là chúng có thể được thao tác trực tiếp, truyền qua hàm, và hơn thế nữa, mà không cần cú pháp đặc biệt hay phương thức truy cập. Quyết định thiết kế này củng cố nhấn mạnh của Clojure vào sự đơn giản và mạnh mẽ.

Dù bản đồ băm cực kỳ hữu ích, cần phải nói rằng đối với các tập dữ liệu lớn hoặc các kịch bản mà khóa liên tục thay đổi (thêm và loại bỏ liên tục), các cấu trúc dữ liệu hoặc cơ sở dữ liệu khác có thể cung cấp hiệu năng và linh hoạt tốt hơn. Tuy nhiên, cho hầu hết các trường hợp sử dụng điển hình trong phạm vi các ứng dụng Clojure, mảng kết hợp cung cấp một phương tiện quản lý dữ liệu mạnh mẽ và hiệu quả.
