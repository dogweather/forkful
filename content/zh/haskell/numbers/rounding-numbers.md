---
date: 2024-01-26 03:45:08.870136-07:00
description: "\u5982\u4F55\u8FDB\u884C\uFF1A Haskell \u4F7F\u7528 `Prelude` \u4E2D\
  \u7684 `round`\u3001`ceiling`\u3001`floor` \u548C `truncate` \u51FD\u6570\u8FDB\u884C\
  \u56DB\u820D\u4E94\u5165\u64CD\u4F5C\u3002"
lastmod: '2024-04-05T22:38:46.972008-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u8FDB\u884C\uFF1A Haskell \u4F7F\u7528 `Prelude` \u4E2D\u7684\
  \ `round`\u3001`ceiling`\u3001`floor` \u548C `truncate` \u51FD\u6570\u8FDB\u884C\
  \u56DB\u820D\u4E94\u5165\u64CD\u4F5C\u3002"
title: "\u6570\u5B57\u53D6\u6574"
weight: 13
---

## 如何进行：
Haskell 使用 `Prelude` 中的 `round`、`ceiling`、`floor` 和 `truncate` 函数进行四舍五入操作。

```haskell
import Prelude

main :: IO ()
main = do
  let num = 3.567
  print $ round num    -- 4
  print $ ceiling num  -- 4
  print $ floor num    -- 3
  print $ truncate num -- 3
  
  -- 四舍五入到特定小数位并不在 Prelude 中。
  -- 这里是一个自定义函数：
  let roundTo n f = (fromInteger $ round $ f * (10^n)) / (10.0^^n)
  print $ roundTo 1 num -- 3.6
```

## 深入了解
历史上，在数值分析和计算机科学中，四舍五入非常重要，因为它对于最小化计算中的误差积累至关重要，特别是在浮点表示标准化为IEEE 754之前。

四舍五入到什么呢？`round` 将你带到最近的整数——向上或向下。`ceiling` 和 `floor` 总是向上或向下四舍五入到最接近的整数，而 `truncate` 仅仅是丢弃小数点。

除了这些函数之外，替代方法可能涉及自定义逻辑，如我们的 `roundTo`，或者你可能会引入库（如 Data.Fixed）以满足更复杂的需求。

注意由于 Haskell 如何处理 `round` 中的一半的情况可能导致的意外结果（它四舍五入到最近的偶数）。

## 另请参阅
- Haskell Prelude 文档中的四舍五入函数：https://hackage.haskell.org/package/base-4.16.1.0/docs/Prelude.html
- Haskell Wiki 上的浮点算术：https://wiki.haskell.org/Floating_point_arithmetic
- IEEE 754-2008 标准，了解更多关于许多语言中如何处理浮点的信息：https://ieeexplore.ieee.org/document/4610935
