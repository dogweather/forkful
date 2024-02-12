---
title:                "Viết các bài kiểm tra"
aliases: - /vi/elixir/writing-tests.md
date:                  2024-01-28T22:13:25.942271-07:00
model:                 gpt-4-0125-preview
simple_title:         "Viết các bài kiểm tra"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/elixir/writing-tests.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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
