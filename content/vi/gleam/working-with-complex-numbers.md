---
title:                "Làm việc với số phức"
date:                  2024-01-28T22:13:00.567877-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với số phức"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/gleam/working-with-complex-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Số phức có một phần thực và một phần ảo (`a + bi`). Chúng rất hữu ích trong nhiều lĩnh vực như kỹ thuật điện và tính toán lượng tử. Các lập trình viên sử dụng chúng để mô hình hóa các phương trình không giải được chỉ bằng số thực.

## Cách thực hiện:
Gleam không hỗ trợ số phức một cách tự nhiên. Bạn thường tự tạo hoặc tìm một thư viện. Dưới đây là một ví dụ nhanh về cách bạn có thể triển khai các thao tác cơ bản:

```gleam
type Complex {
  Complex(Float, Float)
}

fn add(c1: Complex, c2: Complex) -> Complex {
  let Complex(a, b) = c1
  let Complex(x, y) = c2
  Complex(a + x, b + y)
}

fn multiply(c1: Complex, c2: Complex) -> Complex {
  let Complex(a, b) = c1
  let Complex(x, y) = c2
  Complex(a*x - b*y, a*y + b*x)
}

fn main() {
  let num1 = Complex(1.0, 2.0)
  let num2 = Complex(3.0, 4.0)
  let sum = add(num1, num2)
  let product = multiply(num1, num2)

  sum // Complex(4.0, 6.0)
  product // Complex(-5.0, 10.0)
}
```

## Sâu hơn nữa

Số phức được Gerolamo Cardano đầu tiên ghi nhận một cách chính thức vào thế kỷ 16. Chúng là sự mở rộng tự nhiên của các số thực. Tuy nhiên, trong một ngôn ngữ trẻ như Gleam - mà ưu tiên hiệu suất và an toàn kiểu - những tính năng này còn đơn giản (hoặc bạn tự làm).

Trong một số ngôn ngữ khác, như Python, số phức được tích hợp sẵn (`3+4j`), làm cho mọi thứ trở nên dễ dàng hơn. Trong Rust hoặc Haskell, bạn có thư viện cung cấp các chức năng tiên tiến ngay lập tức.

Cách tiếp cận của Gleam có nghĩa là bạn phải xử lý tất cả các khía cạnh: số học, tọa độ cực, hình thức mũ, v.v. Việc triển khai các thao tác chính xác và hiệu quả đòi hỏi phải lập trình cẩn thận, xem xét cách hành vi của số dấu phẩy động có thể ảnh hưởng đến kết quả của bạn.

Hãy nhớ thử nghiệm kỹ lưỡng, đặc biệt là các trường hợp cực đoan! Xử lý vô cực phức tạp và giá trị NaN (không phải là một số) có thể làm bạn lúng túng nếu bạn không cẩn thận.

## Xem thêm
Để biết thêm, đây là nơi bạn có thể khám phá:
- [Tài liệu Chính thức của Gleam](https://gleam.run/documentation/)
- Tìm hiểu thư viện của các ngôn ngữ khác để lấy cảm hứng, như thư viện [num-complex](https://crates.io/crates/num-complex) của Rust hoặc mô-đun [cmath](https://docs.python.org/3/library/cmath.html) của Python.
