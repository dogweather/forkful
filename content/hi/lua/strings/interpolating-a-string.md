---
date: 2024-01-20 17:51:12.582555-07:00
description: "Interpolating a string means inserting dynamic values into a static\
  \ string. Programmers do it to make code flexible and to dynamically create strings\
  \ that\u2026"
lastmod: '2024-03-13T22:44:52.526430-06:00'
model: gpt-4-1106-preview
summary: Interpolating a string means inserting dynamic values into a static string.
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0907\u0902\u091F\u0930\u092A\
  \u094B\u0932\u0947\u0936\u0928"
weight: 8
---

## How to: (कैसे करें:)
Lua doesn't have built-in string interpolation, but we can simulate it using `string.format`:

```Lua
local name = "मोहन"
local age = 30
local greeting = string.format("नमस्ते, मेरा नाम %s है और मेरी उम्र %d है।", name, age)
print(greeting)
```
Output:
```
नमस्ते, मेरा नाम मोहन है और मेरी उम्र 30 है।
```

## Deep Dive (गहराई से जानकारी)
Lua में string interpolation का direct method नहीं है जैसे कुछ और languages में होता है (जैसे Python). `string.format` function का इस्तेमाल करके हम placeholders का उपयोग करते हुए strings में dynamic values को insert कर सकते हैं। `%s` का इस्तेमाल करें जब आपको string value add करनी हो, और `%d` का जब integer values के लिए interpolation करना हो। Lua 5.1 से यह सुविधा available है और यह काफी efficient and reliable तरीका है।

साथ ही, कुछ Lua developers अपने functions बना लेते हैं, or libraries का इस्तेमाल करते हैं जैसे `string.interp` के लिए `Lustache` या `Penlight`। इससे कुछ extra flexibility और power मिलती है, खास कर जब complex templates का काम हो।

## See Also (और जानकारी के लिए)
- Lua `string.format`: [Lua 5.4 Reference Manual](https://www.lua.org/manual/5.4/manual.html#6.4.1)
- `Lustache` template library on GitHub: [Lustache](https://github.com/Olivine-Labs/lustache)
- `Penlight` Lua Libraries: [Penlight](https://github.com/lunarmodules/Penlight)
