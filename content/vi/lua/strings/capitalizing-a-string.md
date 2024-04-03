---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:55:33.801219-07:00
description: "L\xE0m sao: Lua kh\xF4ng c\xF3 h\xE0m vi\u1EBFt hoa s\u1EB5n, v\xEC\
  \ v\u1EADy ch\xFAng ta h\xE3y t\u1EA1o m\u1ED9t h\xE0m."
lastmod: '2024-03-13T22:44:36.800762-06:00'
model: gpt-4-0125-preview
summary: "Lua kh\xF4ng c\xF3 h\xE0m vi\u1EBFt hoa s\u1EB5n, v\xEC v\u1EADy ch\xFA\
  ng ta h\xE3y t\u1EA1o m\u1ED9t h\xE0m."
title: "Vi\u1EBFt hoa m\u1ED9t chu\u1ED7i k\xFD t\u1EF1"
weight: 2
---

## Làm sao:
Lua không có hàm viết hoa sẵn, vì vậy chúng ta hãy tạo một hàm:

```lua
function capitalize(str)
  return (str:gsub("(%l)(%w*)", function(first, rest) return first:upper()..rest end))
end

print(capitalize("hello world"))  -- Đầu ra: Hello World
```

## Thảo luận sâu hơn
Các hàm viết hoa là chuẩn trong nhiều ngôn ngữ lập trình. Trong Lua, chúng ta tạo một hàm sử dụng `string.gsub()`, một hàm phù hợp mẫu mạnh mẽ. Hàm `capitalize` tùy chỉnh của chúng ta sử dụng một mẫu để tìm các chữ cái thường (`%l`) theo sau là không hoặc nhiều ký tự từ (`%w*`), và thay thế chúng bằng chữ cái viết hoa và phần còn lại của từ.

```lua
-- Đây là một cách khác để chỉ viết hoa từ đầu tiên
function capitalizeFirst(str)
  if str == "" then return str end
  return str:sub(1, 1):upper()..str:sub(2)
end
```

Khả năng phù hợp mẫu của Lua không mạnh mẽ như biểu thức chính quy đầy đủ nhưng phù hợp cho nhiều tác vụ thao tác chuỗi. Lưu ý rằng hàm `capitalize` của chúng ta sẽ không viết hoa các từ đi theo sau một số dấu câu nhất định, vì vậy nó không phải là hoàn hảo. Để có giải pháp mạnh mẽ hơn, bạn có thể xem xét thêm phù hợp mẫu hoặc thư viện bên ngoài.

Trong quá khứ, nhu cầu về các hàm viết hoa phát sinh từ mong muốn trình bày dữ liệu văn bản một cách đồng nhất, đặc biệt là trong giao diện người dùng. Tuy nhiên, cần phải hiểu rõ bối cảnh: các ngôn ngữ và văn hóa khác nhau có những quy tắc riêng về viết hoa không chỉ đơn giản là chữ cái đầu của một câu hoặc tên.

## Xem thêm
- Thư viện `string` của Lua: https://www.lua.org/manual/5.4/manual.html#6.4
- Mẫu Lua: https://www.lua.org/pil/20.2.html
- Xử lý Văn bản trong Lua: https://www.lua.org/pil/20.html
