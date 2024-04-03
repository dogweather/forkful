---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:11:04.589535-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Trong Clojure, vi\u1EC7c t\u1EA1o v\xE0\
  \ thao t\xE1c v\u1EDBi m\u1EA3ng k\u1EBFt h\u1EE3p (b\u1EA3n \u0111\u1ED3 b\u0103\
  m) r\u1EA5t \u0111\u01A1n gi\u1EA3n. H\xE3y c\xF9ng t\xECm hi\u1EC3u v\u1EDBi c\xE1\
  c v\xED d\u1EE5. \u0110\u1EC3 t\u1EA1o m\u1ED9t b\u1EA3n \u0111\u1ED3 b\u0103m."
lastmod: '2024-03-13T22:44:36.143395-06:00'
model: gpt-4-0125-preview
summary: "Trong Clojure, vi\u1EC7c t\u1EA1o v\xE0 thao t\xE1c v\u1EDBi m\u1EA3ng k\u1EBF\
  t h\u1EE3p (b\u1EA3n \u0111\u1ED3 b\u0103m) r\u1EA5t \u0111\u01A1n gi\u1EA3n."
title: "S\u1EED d\u1EE5ng m\u1EA3ng li\xEAn k\u1EBFt"
weight: 15
---

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
