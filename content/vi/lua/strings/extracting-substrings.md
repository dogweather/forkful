---
title:                "Trích xuất chuỗi con"
aliases:
- /vi/lua/extracting-substrings.md
date:                  2024-01-28T21:59:50.926513-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trích xuất chuỗi con"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/lua/extracting-substrings.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Việc trích xuất các chuỗi con đồng nghĩa với việc kéo một đoạn cụ thể ra khỏi một chuỗi. Các lập trình viên thực hiện điều này để cô lập, phân tích, hoặc thao tác với dữ liệu cụ thể trong một văn bản lớn hơn.

## Làm thế nào:
Trong Lua, sử dụng hàm `string.sub`:

```lua
local text = "Hello, Lua!"
-- Trích xuất 'Hello'
print(string.sub(text, 1, 5)) -- Đầu ra: Hello

-- Lấy 'Lua'
print(string.sub(text, 8, 11)) -- Đầu ra: Lua
```

Hoặc lấy các ký tự cuối bằng chỉ số âm:

```lua
-- Lấy 'Lua!' từ cuối
print(string.sub(text, -4)) -- Đầu ra: Lua!
```

Sử dụng mẫu để tìm và trích xuất:

```lua
local phrase = "The quick brown fox jumps"
-- Khớp và trích xuất 'quick'
print(phrase:match("(%a+) quick")) -- Đầu ra: The
```

## Sâu hơn
Trong lập trình đầu tiên, việc xử lý chuỗi thủ công và cồng kềnh, thường cần vòng lặp và điều kiện. Hàm `string.sub` của Lua là một phần của thư viện chuỗi phong phú hơn, giúp việc thao tác chuỗi trở nên dễ dàng. Các lựa chọn thay thế cho `string.sub` bao gồm việc khớp mẫu với `string.match`, có thêm sức mạnh nhưng có thể không cần thiết cho các tác vụ đơn giản.

Hàm `string.sub` và việc khớp mẫu đều dựa trên các hàm C do gốc rễ C của Lua. Bạn sẽ không tìm thấy một thư viện tiêu chuẩn rộng lớn trong Lua cho chuỗi so với các ngôn ngữ như Python; nó giữ gìn sự đơn giản và hiệu quả. Nhớ rằng, chỉ số trong Lua bắt đầu từ 1, không phải 0.

## Xem thêm
- Lua 5.4 Reference Manual về Strings: [www.lua.org/manual/5.4/manual.html#6.4](https://www.lua.org/manual/5.4/manual.html#6.4)
- 'Programming in Lua' (ấn bản thứ 4), đặc biệt là chương về chuỗi: [www.lua.org/pil/contents.html](https://www.lua.org/pil/contents.html)
