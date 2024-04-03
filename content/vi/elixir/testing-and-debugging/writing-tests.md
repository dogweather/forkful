---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:25.942271-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Trong Elixir, b\u1EA1n s\u1EBD s\u1EED\
  \ d\u1EE5ng ExUnit \u0111\u1EC3 vi\u1EBFt ki\u1EC3m th\u1EED. \u0110\xF3 l\xE0 m\u1ED9\
  t framework c\xF3 s\u1EB5n, th\xE2n thi\u1EC7n v\xE0 d\u1EC5 d\xE0ng \u0111\u1EC3\
  \ b\u1EAFt \u0111\u1EA7u. D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9t v\xED d\u1EE5\
  \u2026"
lastmod: '2024-03-13T22:44:36.213078-06:00'
model: gpt-4-0125-preview
summary: "Trong Elixir, b\u1EA1n s\u1EBD s\u1EED d\u1EE5ng ExUnit \u0111\u1EC3 vi\u1EBF\
  t ki\u1EC3m th\u1EED."
title: "Vi\u1EBFt c\xE1c b\xE0i ki\u1EC3m tra"
weight: 36
---

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
