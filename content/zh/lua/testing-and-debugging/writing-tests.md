---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:24.997649-07:00
description: "\u5982\u4F55\u8FDB\u884C\uFF1A Lua\u662F\u4E00\u79CD\u8F7B\u91CF\u7EA7\
  \u5374\u5F3A\u5927\u7684\u811A\u672C\u8BED\u8A00\uFF0C\u5B83\u6CA1\u6709\u5185\u7F6E\
  \u7684\u6D4B\u8BD5\u6846\u67B6\u3002\u4F46\u662F\uFF0C\u7B2C\u4E09\u65B9\u5E93\u5982\
  Busted\u548CLuaUnit\u4F7F\u5F97\u6D4B\u8BD5\u76F8\u5BF9\u76F4\u63A5\u3002\u5728\u8FD9\
  \u91CC\uFF0C\u6211\u4EEC\u5C06\u770B\u770B\u4F7F\u7528\u8FD9\u4E24\u8005\u7684\u793A\
  \u4F8B\u3002 #."
lastmod: '2024-03-13T22:44:47.915558-06:00'
model: gpt-4-0125-preview
summary: "Lua\u662F\u4E00\u79CD\u8F7B\u91CF\u7EA7\u5374\u5F3A\u5927\u7684\u811A\u672C\
  \u8BED\u8A00\uFF0C\u5B83\u6CA1\u6709\u5185\u7F6E\u7684\u6D4B\u8BD5\u6846\u67B6\u3002\
  \u4F46\u662F\uFF0C\u7B2C\u4E09\u65B9\u5E93\u5982Busted\u548CLuaUnit\u4F7F\u5F97\u6D4B\
  \u8BD5\u76F8\u5BF9\u76F4\u63A5\u3002\u5728\u8FD9\u91CC\uFF0C\u6211\u4EEC\u5C06\u770B\
  \u770B\u4F7F\u7528\u8FD9\u4E24\u8005\u7684\u793A\u4F8B."
title: "\u7F16\u5199\u6D4B\u8BD5"
weight: 36
---

## 如何进行：
Lua是一种轻量级却强大的脚本语言，它没有内置的测试框架。但是，第三方库如Busted和LuaUnit使得测试相对直接。在这里，我们将看看使用这两者的示例。

### 使用 Busted
Busted是一个受欢迎的Lua测试框架，提供了一种灵活的方式来写测试。首先，通过LuaRocks（Lua的包管理器）用`luarocks install busted`安装Busted。安装后，你就可以编写你的测试了。这里有一个简单的测试，用于一个`add`函数，该函数将两个数字相加：

```lua
-- add.lua
local function add(a, b)
  return a + b
end

return add
```

```lua
-- add_spec.lua
local add = require('add')

describe("Add function", function()
  it("应正确添加两个数字", function()
    assert.are.equal(5, add(2, 3))
  end)
end)
```

要运行测试，在终端执行`busted`。一个通过测试的样本输出看起来像这样：

```
●
1 成功 / 0 失败 / 0 错误 / 0 待定 : 0.002 秒
```

### 使用 LuaUnit
LuaUnit是另一个遵循xUnit惯例的测试框架，设置简单。通过LuaRocks使用`luarocks install luaunit`安装LuaUnit。以下是如何用LuaUnit写一个类似上面的测试：

```lua
-- add.lua 保持不变

-- test_add.lua
luaunit = require('luaunit')
local add = require('add')

function testAdd()
  luaunit.assertEquals(add(2, 3), 5)
end

os.exit(luaunit.LuaUnit.run())
```

直接通过Lua运行这个脚本（`lua test_add.lua`），输出结果可能像这样：

```
.
在0.001秒内运行了1个测试, 1 成功, 0 失败
```

Busted和LuaUnit都提供了广泛的特性来处理各种测试情景，包括模拟、间谍和异步测试。在它们之间的选择取决于您的项目的具体需求以及您对于语法和功能的个人偏好。
