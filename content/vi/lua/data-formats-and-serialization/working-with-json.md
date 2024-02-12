---
title:                "Làm việc với JSON"
aliases:
- /vi/lua/working-with-json.md
date:                  2024-01-28T22:10:45.538194-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/lua/working-with-json.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

JSON (JavaScript Object Notation) được sử dụng để lưu trữ và vận chuyển dữ liệu. Lập trình viên sử dụng JSON vì nó nhẹ, dễ đọc và viết đối với con người, và dễ phân tích và tạo ra đối với máy.

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
