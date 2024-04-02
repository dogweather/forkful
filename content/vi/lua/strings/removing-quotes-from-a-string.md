---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:32.343596-07:00
description: "Lo\u1EA1i b\u1ECF d\u1EA5u ngo\u1EB7c kh\u1ECFi m\u1ED9t chu\u1ED7i\
  \ c\xF3 ngh\u0129a l\xE0 l\u1ED9t b\u1ECF c\xE1c k\xFD t\u1EF1 d\u1EA5u ngo\u1EB7\
  c k\xE9p ho\u1EB7c \u0111\u01A1n \xF4m l\u1EA5y v\u0103n b\u1EA3n c\u1EE7a b\u1EA1\
  n. C\xE1c l\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 l\xE0\
  m s\u1EA1ch\u2026"
lastmod: '2024-03-13T22:44:36.806732-06:00'
model: gpt-4-0125-preview
summary: "Lo\u1EA1i b\u1ECF d\u1EA5u ngo\u1EB7c kh\u1ECFi m\u1ED9t chu\u1ED7i c\xF3\
  \ ngh\u0129a l\xE0 l\u1ED9t b\u1ECF c\xE1c k\xFD t\u1EF1 d\u1EA5u ngo\u1EB7c k\xE9\
  p ho\u1EB7c \u0111\u01A1n \xF4m l\u1EA5y v\u0103n b\u1EA3n c\u1EE7a b\u1EA1n. C\xE1\
  c l\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 l\xE0m s\u1EA1\
  ch\u2026"
title: "Lo\u1EA1i b\u1ECF d\u1EA5u ngo\u1EB7c k\xE9p kh\u1ECFi chu\u1ED7i"
weight: 9
---

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
