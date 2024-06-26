---
changelog:
- 2024-01-30, dogweather, reviewed
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:11:58.574591-07:00
description: "L\xE0m th\u1EBF n\xE0o: Haskell kh\xF4ng c\xF3 m\u1EA3ng k\u1EBFt h\u1EE3\
  p ngay l\u1EADp t\u1EE9c nh\u01B0 m\u1ED9t s\u1ED1 ng\xF4n ng\u1EEF kh\xE1c, nh\u01B0\
  ng n\xF3 cung c\u1EA5p m\u1ED9t th\u01B0 vi\u1EC7n ti\xEAu chu\u1EA9n m\u1EA1nh\
  \ m\u1EBD \u0111\u01B0\u1EE3c g\u1ECDi l\xE0 `Data.Map`\u2026"
lastmod: '2024-03-13T22:44:36.703205-06:00'
model: gpt-4-0125-preview
summary: "Haskell kh\xF4ng c\xF3 m\u1EA3ng k\u1EBFt h\u1EE3p ngay l\u1EADp t\u1EE9\
  c nh\u01B0 m\u1ED9t s\u1ED1 ng\xF4n ng\u1EEF kh\xE1c, nh\u01B0ng n\xF3 cung c\u1EA5\
  p m\u1ED9t th\u01B0 vi\u1EC7n ti\xEAu chu\u1EA9n m\u1EA1nh m\u1EBD \u0111\u01B0\u1EE3\
  c g\u1ECDi l\xE0 `Data.Map` \u0111\u1EC3 l\xE0m vi\u1EC7c v\u1EDBi c\xE1c c\u1EB7\
  p kh\xF3a-gi\xE1 tr\u1ECB."
title: "S\u1EED d\u1EE5ng m\u1EA3ng li\xEAn k\u1EBFt"
weight: 15
---

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
