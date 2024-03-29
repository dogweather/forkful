---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:45.722289-07:00
description: "Vi\u1EC7c vi\u1EBFt hoa m\u1ED9t chu\u1ED7i t\u1EE9c l\xE0 l\xE0m cho\
  \ ch\u1EEF c\xE1i \u0111\u1EA7u ti\xEAn c\u1EE7a m\u1ED9t chu\u1ED7i cho tr\u01B0\
  \u1EDBc tr\u1EDF th\xE0nh ch\u1EEF hoa \u2014 n\u1EBFu \u0111\xF3 l\xE0 m\u1ED9\
  t ch\u1EEF c\xE1i. L\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n vi\u1EC7c\u2026"
lastmod: '2024-03-13T22:44:36.186222-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c vi\u1EBFt hoa m\u1ED9t chu\u1ED7i t\u1EE9c l\xE0 l\xE0m cho ch\u1EEF\
  \ c\xE1i \u0111\u1EA7u ti\xEAn c\u1EE7a m\u1ED9t chu\u1ED7i cho tr\u01B0\u1EDBc\
  \ tr\u1EDF th\xE0nh ch\u1EEF hoa \u2014 n\u1EBFu \u0111\xF3 l\xE0 m\u1ED9t ch\u1EEF\
  \ c\xE1i. L\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n vi\u1EC7c\u2026"
title: "Vi\u1EBFt hoa m\u1ED9t chu\u1ED7i"
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
