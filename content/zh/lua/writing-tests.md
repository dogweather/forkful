---
title:                "编写测试代码"
date:                  2024-01-19
html_title:           "Arduino: 编写测试代码"
simple_title:         "编写测试代码"

category:             "Lua"
tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/lua/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (是什么 & 为什么?)
编写测试是写代码来检查程序的功能是否按预期工作。程序员这样做可以确保代码质量，防止未来更改时出错。

## How to: (如何做)
在Lua中，使用`luaunit`测试库来写测试案例：

```Lua
-- 引入luaunit库
luaunit = require('luaunit')

-- 定义被测试的函数
function isEven(number)
    return number % 2 == 0
end

-- 定义测试用例
function testIsEven()
    luaunit.assertTrue(isEven(2))
    luaunit.assertFalse(isEven(3))
end

-- 运行测试
os.exit(luaunit.LuaUnit.run())
```

运行后输出示例：

```
........
Ran 1 tests in 0.001s

OK
```

## Deep Dive (深入了解)
Lua原生并没有测试框架；`luaunit`是社区提供的解决方案。历史上，Lua更多用于嵌入应用中，测试通常由宿主语言负责。相比之下，`busted`和`Luatest`也是Lua测试的替代方案。关于实现，`luaunit`不需要设置复杂的测试环境，可快速上手，使用简单的断言进行测试。

## See Also (另见)
- `luaunit` GitHub: [https://github.com/bluebird75/luaunit](https://github.com/bluebird75/luaunit)
- `Luatest` GitHub: [https://github.com/tarantool/luatest](https://github.com/tarantool/luatest)
- 官方Lua网站: [http://www.lua.org/](http://www.lua.org/)
