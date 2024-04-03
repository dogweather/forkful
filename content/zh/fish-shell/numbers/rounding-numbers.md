---
date: 2024-01-26 03:45:04.999047-07:00
description: "\u56DB\u820D\u4E94\u5165\u662F\u6307\u53BB\u6389\u5C0F\u6570\u70B9\u540E\
  \u7684\u6570\u5B57\uFF0C\u4EE5\u7B80\u5316\u6570\u636E\u6216\u9002\u5E94\u7279\u5B9A\
  \u683C\u5F0F\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u663E\u793A\
  \u66F4\u53CB\u597D\u3001\u5B58\u50A8\u66F4\u9AD8\u6548\uFF0C\u6216\u5F53\u5C0F\u6570\
  \u7CBE\u5EA6\u4E0D\u662F\u95EE\u9898\u65F6\u3002"
lastmod: '2024-03-13T22:44:48.257282-06:00'
model: gpt-4-0125-preview
summary: "\u56DB\u820D\u4E94\u5165\u662F\u6307\u53BB\u6389\u5C0F\u6570\u70B9\u540E\
  \u7684\u6570\u5B57\uFF0C\u4EE5\u7B80\u5316\u6570\u636E\u6216\u9002\u5E94\u7279\u5B9A\
  \u683C\u5F0F\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u663E\u793A\
  \u66F4\u53CB\u597D\u3001\u5B58\u50A8\u66F4\u9AD8\u6548\uFF0C\u6216\u5F53\u5C0F\u6570\
  \u7CBE\u5EA6\u4E0D\u662F\u95EE\u9898\u65F6\u3002."
title: "\u6570\u5B57\u53D6\u6574"
weight: 13
---

## 如何操作：
在Fish中，四舍五入数字依赖于`math`命令。使用`math -s0`来四舍五入至最近的整数。

```fish
# 向上取整
echo (math -s0 "4.7")
# 输出：5

# 向下取整
echo (math -s0 "4.3")
# 输出：4

# 四舍五入到两位小数
echo (math -s2 "4.5678")
# 输出：4.57

# 负数四舍五入
echo (math -s0 "-2.5")
# 输出：-3
```

## 深入了解
从历史上看，四舍五入数字更多是手动完成的或使用外部工具，但在现代的Shell，如Fish中，它内置于提供的实用程序中。Fish使用`math`命令的方法，与旧的Shell相比简化了操作。在其他编程环境中的替代方式不同；像Python这样的语言使用`round()`函数，而Bash可能需要更复杂的表达式或`bc`实用程序。Fish的四舍五入实现简化了脚本编写，通过保持数学运算在Shell环境内，而不是调用其他工具或语言。

## 另请参阅
- Fish文档中的`math`命令：https://fishshell.com/docs/current/cmds/math.html
- 浮点运算IEEE标准 (IEEE 754)：https://ieeexplore.ieee.org/document/4610935
