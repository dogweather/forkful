---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:50.926513-07:00
description: "Vi\u1EC7c tr\xEDch xu\u1EA5t c\xE1c chu\u1ED7i con \u0111\u1ED3ng ngh\u0129\
  a v\u1EDBi vi\u1EC7c k\xE9o m\u1ED9t \u0111o\u1EA1n c\u1EE5 th\u1EC3 ra kh\u1ECF\
  i m\u1ED9t chu\u1ED7i. C\xE1c l\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n \u0111\
  i\u1EC1u n\xE0y \u0111\u1EC3 c\xF4 l\u1EADp, ph\xE2n t\xEDch, ho\u1EB7c\u2026"
lastmod: '2024-03-13T22:44:36.808004-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c tr\xEDch xu\u1EA5t c\xE1c chu\u1ED7i con \u0111\u1ED3ng ngh\u0129\
  a v\u1EDBi vi\u1EC7c k\xE9o m\u1ED9t \u0111o\u1EA1n c\u1EE5 th\u1EC3 ra kh\u1ECF\
  i m\u1ED9t chu\u1ED7i."
title: "Tr\xEDch xu\u1EA5t chu\u1ED7i con"
weight: 6
---

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
