---
title:                "Sinh số ngẫu nhiên"
date:                  2024-01-28T22:02:04.012305-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sinh số ngẫu nhiên"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/gleam/generating-random-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Trong lập trình, việc sinh số ngẫu nhiên có thể rất quan trọng cho việc tạo ra các mô phỏng, kiểm thử, mã hóa, và trò chơi. Trong Gleam, đây là một tính năng cho phép các nhà phát triển đưa vào sự không chắc chắn hoặc mô phỏng các tình huống thế giới thực trong ứng dụng của họ.

## Làm thế nào:

Để sinh số ngẫu nhiên trong Gleam, bạn chủ yếu sử dụng thư viện `gleam_random`. Thư viện này cung cấp các hàm để sinh số nguyên ngẫu nhiên, số thực nổi, và nhiều hơn nữa. Đầu tiên, đảm bảo bạn đã thêm `gleam_random` vào file `rebar.config` hoặc `mix.exs` của bạn như một sự phụ thuộc.

Hãy thảo luận về một số ví dụ:

### Sinh số nguyên ngẫu nhiên

Để tạo ra một số nguyên ngẫu nhiên trong một phạm vi được chỉ định, bạn có thể sử dụng hàm `int`:

```gleam
import gleam/random

pub fn generate_random_int() {
  let random_int = random.int(1, 10)
  random_int
}
```

Hàm này sẽ sinh ra một số nguyên ngẫu nhiên từ 1 đến 10 bao gồm cả hai.

### Sinh số thực nổi ngẫu nhiên

Để nhận một số thực nổi ngẫu nhiên, sử dụng hàm `float`. Hàm này sinh ra một số thực nổi từ 0.0 đến 1.0:

```gleam
import gleam/random

pub fn generate_random_float() {
  let random_float = random.float()
  random_float
}
```

### Kết quả Ví dụ

Việc chạy các hàm này có thể cho kết quả như:

- Đối với `generate_random_int()`: `5`
- Đối với `generate_random_float()`: `0.84372`

Nhớ là, mỗi lần thực thi có thể dẫn đến các kết quả khác nhau do bản chất của sự ngẫu nhiên.

## Sâu hơn nữa

Mô-đun `gleam_random` thực hiện một bộ sinh số ngẫu nhiên giả (PRNG), có nghĩa là các số không thực sự ngẫu nhiên nhưng khó dự đoán, mô phỏng sự ngẫu nhiên. PRNG hoạt động bằng cách bắt đầu với một giá trị ban đầu, được biết đến như là hạt giống, và áp dụng các phép toán toán học để sinh ra một dãy số.

Lịch sử, các ngôn ngữ và thư viện đã thực hiện một số thuật toán cho PRNGs, như Mersenne Twister hoặc Linear Congruential Generator (LCG). Sự lựa chọn của thuật toán ảnh hưởng đến chất lượng của "sự ngẫu nhiên", với một số phù hợp hơn cho các ứng dụng mã hóa. Mặc dù thư viện chuẩn của Gleam cung cấp sự tiện lợi và dễ sử dụng với mô-đun `gleam_random` của nó, nó có thể không phải lúc nào cũng là lựa chọn tốt nhất cho các trường hợp sử dụng đòi hỏi sự ngẫu nhiên bảo mật mã hóa. Đối với mục đích mã hóa, các nhà phát triển nên tìm hiểu về các thư viện được thiết kế đặc biệt để cung cấp các bộ sinh số ngẫu nhiên giả bảo mật mã hóa (CSPRNG), được thiết kế để chống lại các cuộc tấn công có thể dự đoán số tiếp theo dựa trên việc quan sát một dãy số được sinh ra.

Kết luận, trong khi chức năng sinh số ngẫu nhiên của Gleam là mạnh mẽ cho nhu cầu lập trình chung, các ứng dụng có yêu cầu bảo mật cụ thể nên xem xét giải pháp mã hóa chuyên dụng để đảm bảo tính toàn vẹn và bảo mật của việc sinh số ngẫu nhiên của họ.
