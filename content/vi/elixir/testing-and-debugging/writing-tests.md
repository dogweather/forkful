---
aliases:
- /vi/elixir/writing-tests/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:25.942271-07:00
description: "Vi\u1EBFt ki\u1EC3m th\u1EED trong l\u1EADp tr\xECnh l\xE0 t\u1EA1o\
  \ ra code \u0111\u1EC3 ki\u1EC3m tra xem m\xE3 kh\xE1c c\xF3 ho\u1EA1t \u0111\u1ED9\
  ng \u0111\xFAng c\xE1ch kh\xF4ng. C\xE1c l\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7\
  n \u0111i\u1EC1u n\xE0y \u0111\u1EC3 ph\xE1t hi\u1EC7n s\u1EDBm\u2026"
lastmod: 2024-02-18 23:08:50.372772
model: gpt-4-0125-preview
summary: "Vi\u1EBFt ki\u1EC3m th\u1EED trong l\u1EADp tr\xECnh l\xE0 t\u1EA1o ra code\
  \ \u0111\u1EC3 ki\u1EC3m tra xem m\xE3 kh\xE1c c\xF3 ho\u1EA1t \u0111\u1ED9ng \u0111\
  \xFAng c\xE1ch kh\xF4ng. C\xE1c l\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n \u0111\
  i\u1EC1u n\xE0y \u0111\u1EC3 ph\xE1t hi\u1EC7n s\u1EDBm\u2026"
title: "Vi\u1EBFt c\xE1c b\xE0i ki\u1EC3m tra"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Viết kiểm thử trong lập trình là tạo ra code để kiểm tra xem mã khác có hoạt động đúng cách không. Các lập trình viên thực hiện điều này để phát hiện sớm các lỗi, xác nhận các tính năng mới không làm hỏng các tính năng cũ và ngủ ngon hơn biết rằng mã của họ vững chắc.

## Cách thực hiện:

Trong Elixir, bạn sẽ sử dụng ExUnit để viết kiểm thử. Đó là một framework có sẵn, thân thiện và dễ dàng để bắt đầu. Dưới đây là một ví dụ nhanh:

```elixir
# test/example_test.exs
defmodule ExampleTest do
  use ExUnit.Case

  test "sự thật" do
    assert 1 + 1 == 2
  end
end
```

Chạy nó với `mix test`:

```shell
$ mix test
..

Kết thúc trong 0.03 giây
1 kiểm thử, 0 lỗi
```

Tốt! Bạn đã viết một kiểm thử xác nhận rằng toán học không thay đổi.

## Sâu hơn

Việc kiểm thử đã là một phần quan trọng trong Elixir kể từ khi José Valim tạo ra ngôn ngữ này, lấy cảm hứng từ văn hóa kiểm thử của Ruby. Có sự thay thế nào khác không trong thế giới của Elixir? Không nhiều – ExUnit là lựa chọn hàng đầu. Tuy nhiên, bạn có thể khám phá kiểm thử dựa trên thuộc tính với StreamData hoặc đào sâu vào việc giả mạo với Mox cho các kịch bản phức tạp hơn. Kiểm thử tất cả về việc khẳng định kết quả mong đợi - mà bạn đã thấy với `assert` - nhưng cũng có `refute` để chỉ ra điều gì không nên xảy ra.

## Xem thêm

Để phát triển kỹ năng viết kiểm thử của bạn, hãy kiểm tra những điều này:

- Hướng dẫn kiểm thử của Elixir: https://hexdocs.pm/ex_unit/ExUnit.html
- StreamData cho kiểm thử dựa trên thuộc tính: https://hexdocs.pm/stream_data/StreamData.html
- Giả mạo với Mox: https://hexdocs.pm/mox/Mox.html

Bây giờ, hãy thử viết và kiểm thử mã!
