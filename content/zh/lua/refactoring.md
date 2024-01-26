---
title:                "重构代码"
date:                  2024-01-26T01:47:52.281500-07:00
model:                 gpt-4-0125-preview
simple_title:         "重构代码"
programming_language: "Lua"
category:             "Lua"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/lua/refactoring.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
重构是改善现有代码的结构、可读性和效率而不改变其外部行为的艺术。程序员进行重构是为了使代码更易于维护、降低复杂度，通常还作为添加新功能或修复错误之前的准备步骤。

## 如何进行：
我们来看一个简单的Lua函数，并对其进行重构。我们从一个计算列表中数字总和的函数开始，但该函数编写时没有太多考虑效率或清晰度：

```Lua
function sumList(numbers)
    local result = 0
    for i=1, #numbers do
        for j=1, #numbers do
            if i == j then
                result = result + numbers[i]
            end
        end
    end
    return result
end

print(sumList({1, 2, 3, 4})) -- 输出：10
```

重构为更高效和可读的版本：
```Lua
function sumListRefactored(numbers)
    local result = 0
    for _, value in ipairs(numbers) do
        result = result + value
    end
    return result
end

print(sumListRefactored({1, 2, 3, 4})) -- 依然输出：10
```

重构的版本去除了多余的内部循环，使用`ipairs`来清晰地遍历列表。

## 深入探索
从历史上看，重构起源于80年代末的Smalltalk编程社区，并由Martin Fowler的书《重构：改善既有代码的设计》而普及。在Lua中，重构通常涉及简化复杂的条件判断，将大型函数拆分为小函数，以及优化表的使用以提高性能。

Lua中的重构有其注意事项；Lua的动态性质和灵活的类型系统可能会使某些重构，如重命名变量或更改函数签名，如果不谨慎操作则风险较高。静态代码分析工具（如`luacheck`）可以减少此类风险。另一种方法是测试驱动开发（TDD），在这种开发过程中，代码的持续重构是开发过程的一个不可分割的部分，与单独的重构阶段形成对比。

## 另见
- Roberto Ierusalimschy的《Lua编程》一书，了解最佳实践和示例。
- Martin Fowler的《重构：改善既有代码的设计》一书，了解适用于多种语言的原则。
- LuaRocks目录（https://luarocks.org/）提供维护和重构Lua代码的工具和模块。