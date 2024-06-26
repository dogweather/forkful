---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:38.925182-07:00
description: "\u0110i\u1EC1u g\xEC & T\u1EA1i sao? T\xECm ki\u1EBFm v\xE0 thay th\u1EBF\
  \ v\u0103n b\u1EA3n c\xF3 ngh\u0129a l\xE0 ho\xE1n \u0111\u1ED5i c\xE1c chu\u1ED7\
  i c\u1EE5 th\u1EC3 trong m\u1ED9t kh\u1ED1i v\u0103n b\u1EA3n v\u1EDBi nh\u1EEF\
  ng chu\u1ED7i kh\xE1c. L\u1EADp tr\xECnh vi\xEAn th\u1EF1c\u2026"
lastmod: '2024-04-05T21:53:38.180782-06:00'
model: gpt-4-0125-preview
summary: "T\xECm ki\u1EBFm v\xE0 thay th\u1EBF v\u0103n b\u1EA3n c\xF3 ngh\u0129a\
  \ l\xE0 ho\xE1n \u0111\u1ED5i c\xE1c chu\u1ED7i c\u1EE5 th\u1EC3 trong m\u1ED9t\
  \ kh\u1ED1i v\u0103n b\u1EA3n v\u1EDBi nh\u1EEFng chu\u1ED7i kh\xE1c."
title: "T\xECm ki\u1EBFm v\xE0 thay th\u1EBF v\u0103n b\u1EA3n"
weight: 10
---

### Điều gì & Tại sao?
Tìm kiếm và thay thế văn bản có nghĩa là hoán đổi các chuỗi cụ thể trong một khối văn bản với những chuỗi khác. Lập trình viên thực hiện điều này cho các nhiệm vụ như sửa lỗi, cập nhật thông tin, hoặc định dạng dữ liệu.

### Làm thế nào:
Hàm `string.gsub` của Lua là công cụ không thể thiếu cho việc tìm kiếm và thay thế. Nó hoạt động như sau:

```lua
local text = "The quick brown fox jumps over the lazy dog."
local searchText = "lazy"
local replaceWith = "energetic"

local result = string.gsub(text, searchText, replaceWith)

print(result)
```

Kết quả:

```
The quick brown fox jumps over the energetic dog.
```

Để thay thế TẤT CẢ các lần xuất hiện, `gsub` thực hiện điều này mặc định:

```lua
local text = "Apples are sweet. Apples are juicy."
local result = string.gsub(text, "Apples", "Oranges")

print(result)
```

Kết quả:

```
Oranges are sweet. Oranges are juicy.
```

### Sâu hơn
Tìm kiếm và thay thế văn bản không phải là độc quyền của Lua; đây là tính năng phổ biến trong các ngôn ngữ lập trình. `string.gsub` của Lua quay trở lại với gốc rễ của việc thao tác chuỗi, cung cấp một cách tiếp cận đơn giản để xử lý các mẫu và thay thế.

Về mặt lịch sử, `gsub` (thay thế toàn cầu) được ảnh hưởng bởi lệnh `sed` của Unix và khả năng khớp mẫu mạnh mẽ của Perl. Mặc dù các mẫu của Lua đơn giản hơn so với biểu thức chính quy được tìm thấy trong các ngôn ngữ khác, nhưng vẫn có thể xử lý các trận đấu phức tạp với một chút sáng tạo.

Các phương pháp thay thế cho `string.gsub` bao gồm lặp qua các chuỗi một cách thủ công và xây dựng các thay thế - một phương pháp dễ mắc lỗi hơn. Đối với việc xử lý văn bản nặng, có thể sử dụng các thư viện phân tích cú pháp chuyên dụng.

Về cách thực hiện, `gsub` có thể nhận một hàm như một đối số thay thế cho phép kiểm soát lập trình đối với việc thay thế.

```lua
local result = string.gsub(text, "(%a+)", function(word)
  return #word > 4 and word:upper() or word
end)
```

Đoạn mã này sẽ viết hoa các từ dài hơn bốn ký tự.

### Xem Thêm
- Cuốn sách [Programming in Lua](https://www.lua.org/pil/), cung cấp kiến thức sâu rộng về các khái niệm lập trình Lua.
- Để biết đầy đủ khả năng của mẫu chuỗi Lua, hãy kiểm tra [Lua 5.4 Reference Manual](https://www.lua.org/manual/5.4/manual.html#6.4.1).
