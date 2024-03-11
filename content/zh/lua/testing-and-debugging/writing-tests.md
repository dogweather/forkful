---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:24.997649-07:00
description: "\u5728\u7F16\u7A0B\u4E2D\u7F16\u5199\u6D4B\u8BD5\u6D89\u53CA\u521B\u5EFA\
  \u5C0F\u578B\u3001\u72EC\u7ACB\u7684\u4EE3\u7801\u7247\u6BB5\uFF0C\u4EE5\u81EA\u52A8\
  \u9A8C\u8BC1\u5E94\u7528\u7A0B\u5E8F\u7684\u4E0D\u540C\u90E8\u5206\u662F\u5426\u6309\
  \u9884\u671F\u5DE5\u4F5C\u3002\u5BF9\u4E8ELua\u7A0B\u5E8F\u5458\u6765\u8BF4\uFF0C\
  \u6D4B\u8BD5\u4FDD\u8BC1\u4E86\u53EF\u9760\u6027\uFF0C\u5E76\u6709\u52A9\u4E8E\u4FDD\
  \u6301\u4EE3\u7801\u8D28\u91CF\uFF0C\u52A0\u5FEB\u4E86\u8C03\u8BD5\u8FC7\u7A0B\uFF0C\
  \u4F7F\u4EE3\u7801\u5E93\u4FEE\u6539\u66F4\u52A0\u5B89\u5168\u3002"
lastmod: '2024-03-11T00:14:21.708489-06:00'
model: gpt-4-0125-preview
summary: "\u5728\u7F16\u7A0B\u4E2D\u7F16\u5199\u6D4B\u8BD5\u6D89\u53CA\u521B\u5EFA\
  \u5C0F\u578B\u3001\u72EC\u7ACB\u7684\u4EE3\u7801\u7247\u6BB5\uFF0C\u4EE5\u81EA\u52A8\
  \u9A8C\u8BC1\u5E94\u7528\u7A0B\u5E8F\u7684\u4E0D\u540C\u90E8\u5206\u662F\u5426\u6309\
  \u9884\u671F\u5DE5\u4F5C\u3002\u5BF9\u4E8ELua\u7A0B\u5E8F\u5458\u6765\u8BF4\uFF0C\
  \u6D4B\u8BD5\u4FDD\u8BC1\u4E86\u53EF\u9760\u6027\uFF0C\u5E76\u6709\u52A9\u4E8E\u4FDD\
  \u6301\u4EE3\u7801\u8D28\u91CF\uFF0C\u52A0\u5FEB\u4E86\u8C03\u8BD5\u8FC7\u7A0B\uFF0C\
  \u4F7F\u4EE3\u7801\u5E93\u4FEE\u6539\u66F4\u52A0\u5B89\u5168\u3002"
title: "\u7F16\u5199\u6D4B\u8BD5"
---

{{< edit_this_page >}}

## 什么 & 为什么?

在编程中编写测试涉及创建小型、独立的代码片段，以自动验证应用程序的不同部分是否按预期工作。对于Lua程序员来说，测试保证了可靠性，并有助于保持代码质量，加快了调试过程，使代码库修改更加安全。

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
