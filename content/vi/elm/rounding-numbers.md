---
title:                "Làm tròn số"
date:                  2024-01-28T22:07:18.069462-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm tròn số"

category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/elm/rounding-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Làm gì & Tại sao?

Làm tròn số là việc chỉnh sửa một số thập phân về giá trị nguyên gần nhất hoặc về một số lượng chữ số phân số cụ thể. Các lập trình viên làm tròn số để giảm bớt độ phức tạp, cải thiện tính dễ đọc hoặc đáp ứng yêu cầu về độ chính xác.

## Cách làm:

Mô-đun `Basics` của Elm cung cấp những hàm tiện ích cho việc làm tròn: `round`, `floor`, và `ceiling`. Dưới đây là cách sử dụng chúng.

```elm
import Basics exposing (round, floor, ceiling)

-- Làm tròn về số nguyên gần nhất
round 3.14    --> 3
round 3.5     --> 4

-- Làm tròn xuống
floor 3.999   --> 3

-- Làm tròn lên
ceiling 3.001 --> 4

-- Cắt giảm phần thập phân mà không làm tròn
truncate 3.76 --> 3
```

Elm cũng cung cấp `toLocaleString` để làm tròn đến một số lượng chữ số thập phân cố định:

```elm
import Float exposing (toLocaleString)

-- Làm tròn đến hai chữ số thập phân
toLocaleString 2 3.14159 --> "3.14"
```

## Đi sâu hơn

Elm là một ngôn ngữ lập trình chức năng có kiểu mạnh mẽ, phân chia hiệu ứng phụ ra ngoài cạnh kiến trúc. Điều này đồng nghĩa với việc các hàm như làm tròn phải là pure và dự đoán được. Trong lịch sử, việc làm tròn là một thao tác phổ biến trong nhiều ngôn ngữ lập trình khi đối mặt với sự không chính xác của số học dấu phẩy động.

Cách tiếp cận của Elm đối với việc làm tròn rất đơn giản - các hàm là pure và tuân thủ theo định nghĩa toán học về round, floor, và ceiling. Elm dự đoán nhu cầu phổ biến bằng cách cung cấp các hàm sẵn có, vì việc quản lý độ chính xác là yêu cầu thường xuyên, đặc biệt là trong tài chính và đồ họa.

Các phương án khác cho các hàm sẵn có của Elm có thể bao gồm việc triển khai tùy chỉnh sử dụng các phép toán số học, nhưng điều đó sẽ thêm vào sự phức tạp không cần thiết khi thư viện chuẩn đã thực hiện công việc một cách hiệu quả.

Tính đến phiên bản hiện tại, Elm sử dụng toán số học dấu phẩy động cơ bản của JavaScript cho các thao tác này, do đó, giữ cho sự nhất quán với tiêu chuẩn IEEE 754, điều này là cái cần ghi nhớ khi xem xét về độ chính xác và khả năng lỗi số học dấu phẩy động.

## Xem thêm

- Tài liệu chính thức của mô-đun `Basics` của Elm: https://package.elm-lang.org/packages/elm/core/latest/Basics
- Một cái nhìn chi tiết vào cách hoạt động của số thập phân dấu phẩy động trong máy tính: https://floating-point-gui.de/
- Mô-đun `Float` của Elm cho thêm các thao tác số học dấu phẩy động: https://package.elm-lang.org/packages/elm/core/latest/Float
