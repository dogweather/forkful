---
title:                "Viết hoa một chuỗi"
aliases:
- vi/elixir/capitalizing-a-string.md
date:                  2024-01-28T21:56:45.722289-07:00
model:                 gpt-4-0125-preview
simple_title:         "Viết hoa một chuỗi"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/elixir/capitalizing-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Việc viết hoa một chuỗi tức là làm cho chữ cái đầu tiên của một chuỗi cho trước trở thành chữ hoa — nếu đó là một chữ cái. Lập trình viên thực hiện việc này để đảm bảo tính nhất quán trong định dạng, đánh bóng giao diện người dùng, hoặc tuân thủ các tiêu chuẩn dữ liệu.

## Cách thực hiện:

```elixir
# Viết hoa một chuỗi trong Elixir
string = "elixir programming"
capitalized_string = String.capitalize(string)
IO.puts capitalized_string

# Kết quả sẽ là:
# Elixir programming
```

```elixir
# Viết hoa tất cả các từ trong một chuỗi
string = "elixir programming language"
capitalized_words = String.split(string)
                    |> Enum.map(&String.capitalize/1)
                    |> Enum.join(" ")

IO.puts capitalized_words

# Kết quả sẽ là:
# Elixir Programming Language
```

## Đào sâu

Ngay từ những ngày đầu của ngành công nghiệp máy tính, các ngôn ngữ lập trình thường không chú trọng tới việc thao tác chuỗi như một phần cốt lõi của ngôn ngữ. Tuy nhiên, Elixir lại đi kèm với một mô-đun chức năng chuỗi mạnh mẽ ngay từ đầu, nhờ vào nguồn gốc từ Erlang VM (BEAM) đã được phát triển mạnh mẽ. Việc viết hoa các chuỗi trong Elixir trở nên đơn giản với mô-đun `String`.

Ngoài phương thức `String.capitalize/1` đơn giản, bạn có thể gặp phải các tình huống đòi hỏi hành vi phức tạp hơn. Giả sử bạn cần viết hoa tiêu đề hoặc tên một cách nhạy cảm với văn hóa. Mô-đun `String` của Elixir một mình sẽ không đủ; bạn sẽ tìm đến các thư viện như `Cldr` để hỗ trợ quốc tế hóa.

Phần nền của `String.capitalize/1` tính đến Unicode và các ký tự đa byte, không chỉ là ASCII. Điều này có nghĩa là nó xử lý chính xác một loạt ngôn ngữ và bảng chữ cái, chứ không chỉ là văn bản tiếng Anh.

Là một lựa chọn khác, bạn có thể tự xây dựng chức năng viết hoa của mình, nhưng trong hầu hết các trường hợp, các phương pháp sẵn có sẽ đáp ứng được nhu cầu. Với các thực hiện tuỳ chỉnh, bạn mở ra cánh cửa cho các lỗi tinh tế, đặc biệt là với văn bản quốc tế. Tại sao phải phát minh lại bánh xe khi bạn đã có sẵn các công cụ chất lượng cao?

## Xem thêm

- Tài liệu chính thức `String` của Elixir: [https://hexdocs.pm/elixir/String.html](https://hexdocs.pm/elixir/String.html)
- Elixir School để học thêm về chuỗi và các kiến thức cơ bản khác: [https://elixirschool.com/en/lessons/basics/strings/](https://elixirschool.com/en/lessons/basics/strings/)
- Thư viện ExCldr cho hỗ trợ quốc tế hóa: [https://hex.pm/packages/ex_cldr](https://hex.pm/packages/ex_cldr)
