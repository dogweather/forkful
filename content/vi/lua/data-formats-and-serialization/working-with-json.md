---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:45.538194-07:00
description: "L\xE0m th\u1EBF n\xE0o: H\xE3y ph\xE2n t\xEDch m\u1ED9t s\u1ED1 JSON."
lastmod: '2024-03-13T22:44:36.849607-06:00'
model: gpt-4-0125-preview
summary: "H\xE3y ph\xE2n t\xEDch m\u1ED9t s\u1ED1 JSON."
title: "L\xE0m vi\u1EC7c v\u1EDBi JSON"
weight: 38
---

## Làm thế nào:
Hãy phân tích một số JSON.

```lua
-- Đảm bảo bạn có mô đun 'dkjson' hoặc một thư viện JSON khác.
local dkjson = require 'dkjson'

local jsonString = '{"name":"John", "age":30, "city":"New York"}'

-- Phân tích chuỗi JSON thành bảng Lua.
local person, pos, err = dkjson.decode(jsonString, 1, nil)
if err then
    print("Lỗi:", err)
else
    print(person.name)  -- Đầu ra: John
end

-- Chuyển đổi bảng Lua thành chuỗi JSON.
local personTable = { name = "Jane", age = 25, city = "Los Angeles" }
local jsonOutput = dkjson.encode(personTable)
print(jsonOutput)  -- Đầu ra: {"age":25,"city":"Los Angeles","name":"Jane"}
```

Bây giờ hãy xử lý các mảng.

```lua
local jsonArrayString = '[{"name":"John"}, {"name":"Jane"}]'

-- Phân tích chuỗi JSON có mảng thành bảng Lua.
local peopleArray, _, err = dkjson.decode(jsonArrayString)
if err then
    print("Lỗi:", err)
else
    for i, person in ipairs(peopleArray) do
        print(person.name)  -- Đầu ra: John\nJane
    end
end
```

## Đào sâu
JSON trở thành tiêu chuẩn de facto cho các API, vượt qua XML vì nó ít dài dòng hơn. Có các lựa chọn khác như YAML, thậm chí đọc dễ hơn nhưng không được sử dụng rộng rãi trong các API. Trong Lua, không có hỗ trợ JSON bản địa, vì vậy bạn cần một thư viện như 'dkjson' hoặc 'cjson'. Các chi tiết cài đặt Lua bao gồm xử lý sự khác biệt về kiểu, như mảng và đối tượng, và chuyển đổi giữa `nil` của Lua và `null` của JSON.

## Xem Thêm
- [Thư viện dkjson trên GitHub](https://github.com/LuaDist/dkjson)
- [Trang web chính thức của JSON](https://www.json.org/json-en.html)
- [Lập Trình trong Lua (phiên bản đầu tiên)](https://www.lua.org/pil/contents.html) để học các kiến thức cơ bản của Lua.
