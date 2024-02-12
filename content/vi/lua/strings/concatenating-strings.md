---
title:                "Nối chuỗi ký tự"
date:                  2024-01-28T21:57:13.753230-07:00
model:                 gpt-4-0125-preview
simple_title:         "Nối chuỗi ký tự"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/lua/concatenating-strings.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại sao?
Nối chuỗi nghĩa là ghép chúng lại với nhau từ đầu đến cuối để tạo thành một chuỗi mới. Lập trình viên làm điều này để xây dựng văn bản một cách động, như tạo tin nhắn hoặc sinh mã.

## Cách thực hiện:
Trong Lua, bạn nối chuỗi bằng toán tử `..`. Hãy xem nó hoạt động như thế nào:

```lua
local hello = "Hello, "
local world = "world!"
local greeting = hello .. world

print(greeting)  -- Output: Hello, world!
```

Bạn thậm chí còn có thể gắn thêm số với một chút ép kiểu:

```lua
local base = "Tôi có "
local itemCount = 3
local message = base .. itemCount .. " quả táo"

print(message)  -- Output: Tôi có 3 quả táo
```

Nhớ là, việc chuyển đổi các kiểu không phải chuỗi là thủ công:

```lua
local score = 9001
local displayScore = "Điểm của bạn là: " .. tostring(score)

print(displayScore)  -- Output: Điểm của bạn là: 9001
```

## Sâu hơn
Nối chuỗi có vẻ tầm thường, nhưng nó thực sự quan trọng. Trong những ngày đầu của Lua, nó được thiết kế cho các hệ thống nhúng, điều đó có nghĩa là giữ cho mọi thứ nhẹ nhàng. Đó là lý do tại sao `..` được chọn cho chuỗi - nó đơn giản mà hiệu quả.

Các phương án khác cho `..` bao gồm:

- Hàm `table.concat` dành cho mảng chuỗi, hiệu quả hơn khi nối nhiều chuỗi.
- Các hàm thư viện chuỗi như `string.format`, cung cấp nhiều kiểm soát hơn đối với định dạng.

Hiệu suất nối chuỗi của Lua là một mối quan tâm, cụ thể là với `..` vì mỗi lần sử dụng tạo ra một chuỗi mới, có thể tốn kém trong các vòng lặp. Để giảm thiểu điều này, khi nối trong một vòng lặp, sử dụng bảng:

```lua
local parts = {}
for i = 1, 10 do
    parts[i] = "Phần " .. i
end
local combined = table.concat(parts, ", ")

print(combined)  -- Output: Phần 1, Phần 2, ... Phần 10
```

Nội bộ, Lua quản lý chuỗi trong một bảng băm để tối ưu hóa việc sử dụng bộ nhớ, vì vậy các chuỗi giống nhau chia sẻ cùng một vùng lưu trữ. Tuy nhiên, việc nối chuỗi phá vỡ sự chia sẻ này do các chuỗi mới nó tạo ra.

## Xem Thêm
- Tài liệu chính thức của Lua về chuỗi: https://www.lua.org/manual/5.4/manual.html#6.4
- Lập trình trong Lua (Sách): https://www.lua.org/pil/contents.html
- Mẹo thao tác chuỗi: https://lua-users.org/wiki/StringLibraryTutorial
