---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:22.929675-07:00
description: "L\xE0m vi\u1EC7c v\u1EDBi CSV (Gi\xE1 Tr\u1ECB Ng\u0103n C\xE1ch B\u1EB1\
  ng D\u1EA5u Ph\u1EA9y) c\xF3 ngh\u0129a l\xE0 ph\xE2n t\xEDch c\xFA ph\xE1p v\xE0\
  \ t\u1EA1o d\u1EEF li\u1EC7u v\u0103n b\u1EA3n \u0111\u01B0\u1EE3c ph\xE2n t\xE1\
  ch b\u1EB1ng d\u1EA5u ph\u1EA9y. C\xE1c l\u1EADp tr\xECnh vi\xEAn\u2026"
lastmod: '2024-03-13T22:44:36.850915-06:00'
model: gpt-4-0125-preview
summary: "L\xE0m vi\u1EC7c v\u1EDBi CSV (Gi\xE1 Tr\u1ECB Ng\u0103n C\xE1ch B\u1EB1\
  ng D\u1EA5u Ph\u1EA9y) c\xF3 ngh\u0129a l\xE0 ph\xE2n t\xEDch c\xFA ph\xE1p v\xE0\
  \ t\u1EA1o d\u1EEF li\u1EC7u v\u0103n b\u1EA3n \u0111\u01B0\u1EE3c ph\xE2n t\xE1\
  ch b\u1EB1ng d\u1EA5u ph\u1EA9y. C\xE1c l\u1EADp tr\xECnh vi\xEAn\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi CSV"
---

{{< edit_this_page >}}

## Cái Gì và Tại Sao?

Làm việc với CSV (Giá Trị Ngăn Cách Bằng Dấu Phẩy) có nghĩa là phân tích cú pháp và tạo dữ liệu văn bản được phân tách bằng dấu phẩy. Các lập trình viên thực hiện điều này vì sự đơn giản và khả năng tương thích - hầu như mọi hệ thống và ngôn ngữ đều hỗ trợ CSV, làm cho nó trở thành lựa chọn không cần suy nghĩ cho trao đổi dữ liệu.

## Làm Thế Nào:

Hãy đọc và viết các tệp CSV bằng Lua. Chúng tôi sẽ xử lý một ví dụ cơ bản mà không cần thư viện bên ngoài.

**Đọc một tệp CSV:**

```Lua
function read_csv(filepath)
  local results = {}
  local file = assert(io.open(filepath, "r"))
  
  for line in file:lines() do
    table.insert(results, line:split(","))
  end
  
  file:close()
  return results
end

-- Một hàm trợ giúp để tách chuỗi
function string:split(delimiter)
  local result = {}
  local from = 1
  local delim_from, delim_to = self:find(delimiter, from, true)
  while delim_from do
    table.insert(result, self:sub(from, delim_from - 1))
    from = delim_to + 1
    delim_from, delim_to = self:find(delimiter, from, true)
  end
  table.insert(result, self:sub(from))
  return result
end
```

**Viết vào một tệp CSV:**

```Lua
function write_csv(filepath, data)
  local file = assert(io.open(filepath, "w"))
  
  for _, row in ipairs(data) do
    file:write(table.concat(row, ",") .. "\n")
  end
  
  file:close()
end

-- Dữ liệu mẫu
local data = {
  { "Tên", "Tuổi", "Thành Phố" },
  { "Alice", "30", "New York" },
  { "Bob", "25", "Los Angeles" }
}

write_csv("output.csv", data)
```

## Sâu Hơn

Lịch sử của CSV có từ những ngày đầu của công nghệ thông tin, nơi sự đơn giản là chìa khóa. Mặc dù JSON và XML hiện cung cấp các cấu trúc dữ liệu phong phú hơn, CSV vẫn được ưa chuộng do khả năng đọc và dễ dàng chỉnh sửa bằng phần mềm bảng tính. Khi triển khai, hãy chú ý đến các trường có dấu phẩy, dòng mới hoặc dấu nháy – những trường đó nên được đặt trong dấu nháy và/hoặc được thoát ra một cách thích hợp.

## Xem Thêm

- Sổ tay tham khảo chính thức Lua 5.4: https://www.lua.org/manual/5.4/
- RFC 4180, Định dạng chung và loại MIME cho các tệp Giá Trị Ngăn Cách bởi Dấu Phẩy (CSV): https://tools.ietf.org/html/rfc4180
- Thư viện Lua Penlight (cho việc xử lý CSV tiên tiến hơn): https://github.com/lunarmodules/Penlight
