---
title:                "Làm tròn số"
date:                  2024-01-28T22:07:04.650885-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm tròn số"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/gleam/rounding-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Làm gì & Tại sao?
Làm tròn số là việc điều chỉnh một giá trị để gần với số được chỉ định nhất—như từ 2.56 thành 3 nếu chúng ta làm tròn thành số nguyên. Lập trình viên thực hiện việc này để đơn giản hóa hoặc để đáp ứng một số thông số kỹ thuật nhất định, thường là để tránh những tinh tế do lỗi chính xác số dấu phẩy động gây ra hoặc để làm cho đầu ra thân thiện với người dùng.

## Cách thực hiện:
Trong Gleam, việc làm tròn không có trong thư viện chuẩn tính đến lần kiểm tra cuối cùng của tôi, nhưng đây là cách bạn thông thường làm tròn một số thực thành số nguyên gần nhất sử dụng trực tiếp các hàm Erlang:

```gleam
external fn erlang_round(Float) -> Int = "erlang" "round"

pub fn main() {
  let rounded = erlang_round(2.56)
  rounded // Đầu ra: 3
}
```

Đầu ra:
```
3
```

Bạn có ý tưởng về độ chính xác khác biệt không? Ví dụ, làm tròn đến hai chữ số thập phân? Chúng ta cần một chút toán học:

```gleam
pub fn round_to_two_places(num: Float) -> Float {
  let multiplier = 100.0
  let tmp = num * multiplier
  let round_tmp = erlang_round(tmp)
  round_tmp / multiplier
}

pub fn main() {
    round_to_two_places(2.569) // Đầu ra: 2.57
}
```

Đầu ra:
```
2.57
```

## Sâu hơn nữa
Trong lịch sử, việc làm tròn số luôn rất quan trọng, đặc biệt trong các tính toán tài chính và khoa học nơi mà độ chính xác và chuẩn mực rất quan trọng. Không có việc làm tròn, bạn sẽ gặp phải những số thập phân dài khó chịu mọi nơi, làm cho các phép toán trở nên không khả thi và dễ phạm lỗi.

Trong thế giới lập trình, các ngôn ngữ khác nhau cung cấp những cách tiếp cận khác nhau, từ các hàm tích hợp sẵn đến các thư viện toán học toàn diện. Việc làm tròn có thể liên quan đến các quy tắc khác nhau – ví dụ, "làm tròn lên nửa" (phương pháp thường dùng) hoặc "làm tròn nửa thành chẵn" (thường được sử dụng trong các tính toán tài chính để tránh thiên vị).

Gleam, khi là một ngôn ngữ mới với nguồn gốc từ Erlang, dựa vào bộ sưu tập hàm số học vững chắc của Erlang. Khi ngôn ngữ phát triển, chúng ta có thể sẽ thấy các hàm gốc được giới thiệu, giảm bớt nhu cầu gọi các thủ tục bên ngoài.

## Xem Thêm
- Mô đun :math của Erlang cho nhiều hoạt động tính toán số học hơn: https://erlang.org/doc/man/math.html
- Để hiểu tại sao việc làm tròn có thể trở nên phức tạp, Tiêu chuẩn Điểm Nổi IEEE: https://ieeexplore.ieee.org/document/8766229
- Quan tâm đến toán học đằng sau điều này? Kiểm tra "Những gì Mọi Nhà Khoa Học Máy Tính Nên Biết Về Số Học Dấu Phẩy Động": https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html
