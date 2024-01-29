---
title:                "Tìm kiếm và thay thế văn bản"
date:                  2024-01-28T22:07:23.909145-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tìm kiếm và thay thế văn bản"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/gleam/searching-and-replacing-text.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Việc tìm kiếm và thay thế văn bản liên quan đến việc tìm kiếm các chuỗi cụ thể trong một khối văn bản lớn hơn và đổi chúng thành các chuỗi khác. Lập trình viên thực hiện điều này cho các nhiệm vụ như cập nhật mã, sửa lỗi, hoặc xử lý các định dạng dữ liệu.

## Cách thực hiện:

Trong Gleam, hãy giữ nó đơn giản. Bạn muốn tìm kiếm "world" và thay thế nó bằng "Gleam". Dưới đây là mã:

```gleam
import gleam/string

pub fn replace_world_with_gleam(input: String) -> String {
  string.replace(input, "world", "Gleam")
}

pub fn main() {
  let text = "Hello, world!"
  let new_text = replace_world_with_gleam(text)
  new_text
}
```

Kết quả mẫu:

```
"Hello, Gleam!"
```

Không rắc rối, không lằng nhằng – hoạt động như một điều kỳ diệu.

## Sâu hơn nữa

Trong lịch sử, việc tìm kiếm và thay thế văn bản đã cũ như trái đất trong các thuật ngữ lập trình. Nó cơ bản nhưng thiết yếu, giống như cây vít dành cho thợ mộc. Trong gia đình chức năng, chúng tôi luôn quan tâm đến chuỗi không có tác dụng phụ. Trong Gleam, mô-đun `string` là nơi bạn cần đến, nó được tích hợp sẵn và sẵn sàng hoạt động. Không cần phải phát minh lại bánh xe ở đây.

Có phương án thay thế? Chắc chắn, bạn có thể sử dụng biểu thức chính quy nếu nhu cầu của bạn trở nên phức tạp, hoặc có thể tìm hiểu về các thư viện hoặc ngôn ngữ khác nhau, nhưng đối với nhiều nhiệm vụ, `string.replace` của Gleam đánh trúng mục tiêu. Nó gọn gàng, có chức năng (Gleam là một ngôn ngữ mạnh mẽ, tĩnh, thuộc gia đình ML) và nó kết nối vào hệ sinh thái BEAM – giống như Erlang và Elixir.

Khi bạn chạy `string.replace`, bạn kích hoạt một chuỗi so sánh mẫu ký tự bên dưới. Nó hiệu quả và làm được công việc mà bạn không cần lo lắng về những chi tiết nhỏ nhặt. Nguyên tắc hướng dẫn của Gleam là giữ mọi thứ an toàn theo kiểu và ngắn gọn – nó cũng đúng với việc thao tác chuỗi.

## Xem thêm

- Tài liệu mô-đun String của Gleam: https://hexdocs.pm/gleam_stdlib/gleam/string/
- Biểu thức chính quy trong Gleam với thư viện `gleam_regex`: https://hex.pm/packages/gleam_regex
- Để hiểu rõ hơn, xem triết lý chung của Gleam: https://gleam.run/book/tour/philosophy.html

Cho dù nhiệm vụ của bạn là đơn giản hay phức tạp, Gleam đều có sẵn để hỗ trợ cho mọi việc liên quan đến chuỗi. Chúc bạn lập trình vui vẻ!
