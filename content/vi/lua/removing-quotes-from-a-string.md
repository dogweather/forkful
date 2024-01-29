---
title:                "Loại bỏ dấu ngoặc kép khỏi chuỗi"
date:                  2024-01-28T22:06:32.343596-07:00
model:                 gpt-4-0125-preview
simple_title:         "Loại bỏ dấu ngoặc kép khỏi chuỗi"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/lua/removing-quotes-from-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?
Loại bỏ dấu ngoặc khỏi một chuỗi có nghĩa là lột bỏ các ký tự dấu ngoặc kép hoặc đơn ôm lấy văn bản của bạn. Các lập trình viên làm điều này để làm sạch đầu vào, để dễ dàng phân tích, hoặc để hòa hợp dữ liệu có thể được trích dẫn không nhất quán.

## Cách làm:
Dưới đây là cách để loại bỏ các dấu ngoặc trong Lua:

```lua
local function remove_quotes(str)
  return (str:gsub("^%p(.*)%p$", "%1"))
end

print(remove_quotes('"Hello, World!"'))     -- Hello, World!
print(remove_quotes("'Goodbye, Quotes!'"))  -- Goodbye, Quotes!
```

Bingo! Những dấu ngoặc ấy biến mất như tất trong máy sấy.

## Sâu hơn
Mọi người đã được loại bỏ dấu ngoặc khỏi chuỗi từ khi ngôn ngữ có thể xử lý văn bản, đó là khá lâu. Trong Lua, hàm `gsub` làm phần việc nặng nhọc, sử dụng các mẫu như một dao mổ để loại bỏ dấu ngoặc. Các phương án khác? Chắc chắn, bạn có thể sử dụng regex trong các ngôn ngữ hỗ trợ nó, hoặc viết vòng lặp riêng của bạn qua từng ký tự (buồn ngủ, nhưng ei, đó là thời gian của bạn).

Khả năng tương ứng mẫu của Lua mang lại cho bạn trải nghiệm regex-lite mà không cần nhập một thư viện toàn bộ. Dấu mũ (`^`) và ký hiệu đô la (`$`) tương ứng với việc khớp vị trí bắt đầu và kết thúc của chuỗi; `%p` khớp với bất kỳ ký tự dấu câu nào. Sau khi lấy bỏ dấu câu ở đầu và cuối, chúng ta thu được tất cả mọi thứ khác với `(.*),` và thay thế toàn bộ phần khớp đó với nhóm thu lại được sử dụng `" %1"`.

Hãy nhớ khả năng tương ứng mẫu của Lua không mạnh mẽ như các động cơ regex đầy đủ – chẳng hạn, nó không thể đếm hoặc quay lui. Sự đơn giản này vừa là một lợi ích vừa là một rắc rối, tùy thuộc vào bạn đang quản lý dấu ngoặc nào và chúng ẩn náu ở đâu.

## Xem thêm
Đắm chìm sâu hơn vào khả năng tương ứng mẫu của Lua với sách PiL (Lập trình trong Lua): http://www.lua.org/pil/20.2.html

Để thấy sự tinh tế tối đa, hãy xem cách các ngôn ngữ khác thực hiện để so sánh, bắt đầu với `str.strip` của Python: https://docs.python.org/3/library/stdtypes.html#str.strip
