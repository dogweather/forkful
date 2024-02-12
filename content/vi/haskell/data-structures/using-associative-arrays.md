---
title:                "Sử dụng mảng liên kết"
aliases: - /vi/haskell/using-associative-arrays.md
date:                  2024-01-30T19:11:58.574591-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng mảng liên kết"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/haskell/using-associative-arrays.md"
changelog:
  - 2024-01-30, dogweather, reviewed
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Mảng kết hợp, hay còn được biết đến với tên là từ điển, trong Haskell là về việc ánh xạ các khóa với các giá trị để tìm kiếm nhanh chóng và quản lý dữ liệu hiệu quả. Lập trình viên sử dụng chúng để xử lý các bộ sưu tập của các phần tử ghép đôi, nơi mà việc tìm kiếm một phần tử trở nên dễ dàng, so với việc sử dụng danh sách.

## Làm thế nào:

Haskell không có mảng kết hợp ngay lập tức như một số ngôn ngữ khác, nhưng nó cung cấp một thư viện tiêu chuẩn mạnh mẽ được gọi là `Data.Map` để làm việc với các cặp khóa-giá trị. Hãy cán lên tay áo và xem cách sử dụng chúng!

Đầu tiên, đảm bảo rằng bạn đã nhập nó:
```Haskell
import qualified Data.Map as Map
```

Việc tạo một map khá đơn giản. Hãy tạo một với một số ngôn ngữ lập trình và các mô hình của chúng:
```Haskell
let languages = Map.fromList [("Haskell", "Hàm số"), ("Python", "Mệnh lệnh"), ("Prolog", "Logic")]
```

Bây giờ, làm thế nào về việc lấy mô hình của Haskell?
```Haskell
Map.lookup "Haskell" languages
-- output: Just "Hàm số"
```

Thêm một ngôn ngữ mới dễ dàng:
```Haskell
let languagesUpdated = Map.insert "Rust" "Hệ thống" languages
```

Nếu chúng ta muốn liệt kê tất cả các ngôn ngữ? Sử dụng `Map.keys`:
```Haskell
Map.keys languagesUpdated
-- output: ["Haskell","Python","Prolog","Rust"]
```

Để liệt kê các mô hình, sử dụng `Map.elems`:
```Haskell
Map.elems languagesUpdated
-- output: ["Hàm số","Mệnh lệnh","Logic","Hệ thống"]
```

Những thao tác cơ bản này nên đủ để đáp ứng hầu hết các nhu cầu sử dụng, nhưng còn rất nhiều điều để khám phá trong `Data.Map`!

## Sâu hơn

Module `Data.Map` trong thư viện tiêu chuẩn của Haskell được xây dựng dựa trên các cây nhị phân cân bằng, cụ thể là cây AVL. Sự lựa chọn này đảm bảo rằng hầu hết các thao tác trên bản đồ, như chèn, xóa và tìm kiếm, có thể được thực hiện trong thời gian O(log n), nơi n là số lượng các phần tử trong bản đồ. Đây là một lựa chọn hiệu quả cho nhiều trường hợp sử dụng, mặc dù không phải là nhanh nhất cho tất cả các tình huống.

Có một sự tinh tế của lịch sử cũng: trước khi `Data.Map` trở thành lựa chọn hàng đầu, các lập trình viên Haskell thường sử dụng các danh sách các cặp để mô phỏng mảng kết hợp. Tuy nhiên, các thao tác trên những cấu trúc như vậy là O(n) cho việc tìm kiếm, khiến `Data.Map` trở thành một cải tiến đáng kể về hiệu suất.

Bây giờ, mặc dù hiệu quả và tiện ích của `Data.Map`, nó không phải lúc nào cũng là công cụ tốt nhất cho mọi công việc. Đối với các nhiệm vụ cực kỳ nhạy cảm với hiệu suất, nơi ngay cả thời gian tìm kiếm O(log n) cũng quá chậm, hoặc nơi mà các khóa luôn là giá trị số nguyên, mảng hoặc bảng băm (thông qua `Data.HashMap`) có thể cung cấp hiệu suất tốt hơn với thời gian truy cập O(1).

Hệ sinh thái Haskell cho phép sử dụng một loạt các cấu trúc dữ liệu ứng với các nhu cầu khác nhau, và `Data.Map` là một lựa chọn chung chung tốt cho mảng kết hợp, cân bằng giữa sự dễ sử dụng, linh hoạt và hiệu suất.
