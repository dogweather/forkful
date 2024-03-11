---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:02.314867-07:00
description: "Ki\u1EC3m tra n\u1EBFu m\u1ED9t th\u01B0 m\u1EE5c t\u1ED3n t\u1EA1i\
  \ c\xF3 ngh\u0129a l\xE0 x\xE1c minh s\u1EF1 c\xF3 m\u1EB7t c\u1EE7a m\u1ED9t th\u01B0\
  \ m\u1EE5c trong h\u1EC7 th\u1ED1ng t\u1EC7p. C\xE1c l\u1EADp tr\xECnh vi\xEAn l\xE0\
  m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 tr\xE1nh c\xE1c l\u1ED7i nh\u01B0\u2026"
lastmod: '2024-03-11T00:14:10.128093-06:00'
model: gpt-4-0125-preview
summary: "Ki\u1EC3m tra n\u1EBFu m\u1ED9t th\u01B0 m\u1EE5c t\u1ED3n t\u1EA1i c\xF3\
  \ ngh\u0129a l\xE0 x\xE1c minh s\u1EF1 c\xF3 m\u1EB7t c\u1EE7a m\u1ED9t th\u01B0\
  \ m\u1EE5c trong h\u1EC7 th\u1ED1ng t\u1EC7p. C\xE1c l\u1EADp tr\xECnh vi\xEAn l\xE0\
  m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 tr\xE1nh c\xE1c l\u1ED7i nh\u01B0\u2026"
title: "Ki\u1EC3m tra xem th\u01B0 m\u1EE5c c\xF3 t\u1ED3n t\u1EA1i kh\xF4ng"
---

{{< edit_this_page >}}

## Cái Gì và Tại Sao?

Kiểm tra nếu một thư mục tồn tại có nghĩa là xác minh sự có mặt của một thư mục trong hệ thống tệp. Các lập trình viên làm điều này để tránh các lỗi như cố gắng đọc từ hoặc viết vào một vị trí không tồn tại, có thể làm sập chương trình hoặc hỏng dữ liệu.

## Làm Thế Nào:

Lua không có xử lý thư mục được tích hợp sẵn trong các thư viện chuẩn của nó. Bạn thường sử dụng `os.execute` với `test` trên Unix hoặc `os.getenv` trên Windows. Dưới đây là cách bạn làm:

```lua
local function is_directory_exists(path)
    if package.config:sub(1,1) == '\\' then -- kiểm tra cho Windows
        local cd_result = os.execute('cd ' .. path .. ' 2>nul')
        return cd_result == true or cd_result == 0
    else -- giả sử Unix-like
        local test_result = os.execute('[ -d "' .. path .. '" ]')
        return test_result == true or test_result == 0
    end
end

print(is_directory_exists("/path/to/check/")) -- Hệ thống Unix-like
print(is_directory_exists("C:\\path\\to\\check\\")) -- Hệ thống Windows
```

Kết quả mẫu có thể đơn giản là `true` nếu thư mục tồn tại hoặc `false` nếu không.

## Sâu Hơn

Trong thời đại máy tính đầu tiên, quản lý tệp là quan trọng trong các hệ điều hành, và việc kiểm tra sự tồn tại của thư mục là trực tiếp trong các lệnh shell. Lua, mặc dù được thiết kế để nhúng và mở rộng, vẫn giữ tính tối giản và do đó dựa vào các lệnh bên ngoài cho những nhiệm vụ như vậy.

Hàm `os.execute` của Lua thực thi một lệnh hệ thống, làm cho nó đa dụng cho mục đích này. Các hệ thống dựa trên Unix phản hồi tốt với cờ `-d` kiểm tra thư mục. Trên Windows, việc cố gắng thay đổi thư mục bằng `cd` phục vụ cho việc kiểm tra của chúng ta.

Có các phương án thay thế như thư viện `lfs` (LuaFileSystem) cung cấp `lfs.attributes(path, "mode")`, một phương pháp mạnh mẽ và dễ đọc hơn để làm điều tương tự, nhưng nó yêu cầu cài đặt các phụ thuộc bổ sung.

Vì lý do về hiệu suất, các lệnh gọi hệ thống trực tiếp có thể nhanh hơn việc bao gồm một thư viện đầy đủ, đặc biệt là cho những nhiệm vụ đơn giản như kiểm tra sự tồn tại của một thư mục. Tuy nhiên, sử dụng `os.execute` có gánh nặng từ việc tạo một quy trình mới, vì vậy hãy cảnh giác trong một vòng lặp chật hẹp.

## Tham Khảo

- Tài liệu LuaFileSystem: http://keplerproject.github.io/luafilesystem/manual.html
- Tham khảo thư viện `os` của Lua: https://www.lua.org/manual/5.4/manual.html#6.9
- "Programming in Lua" để hiểu sâu hơn về ngôn ngữ: https://www.lua.org/pil/
