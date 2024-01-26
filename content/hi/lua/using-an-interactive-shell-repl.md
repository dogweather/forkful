---
title:                "इंटरैक्टिव शेल (REPL) का उपयोग"
date:                  2024-01-26T04:16:37.639513-07:00
model:                 gpt-4-0125-preview
simple_title:         "इंटरैक्टिव शेल (REPL) का उपयोग"
programming_language: "Lua"
category:             "Lua"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/lua/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
REPL का मतलब है Read-Eval-Print Loop, एक इंटरैक्टिव वातावरण जहां आप तेजी से कोड का परीक्षण कर सकते हैं। प्रोग्रामर इसका उपयोग प्रयोग करने, डिबगिंग, और भाषा की विशेषताओं को सीखने के लिए करते हैं।

## कैसे करें:
Lua के REPL में जाने के लिए, बस अपने टर्मिनल में `lua` दर्ज करें। यहाँ एक उदाहरण सत्र है:

```Lua
> x = 10
> print(x * 2)
20
> t = {'apple', 'banana', 'cherry'}
> table.insert(t, 'date')
> for i, fruit in ipairs(t) do print(i, fruit) end
1	apple
2	banana
3	cherry
4	date
>
```
सत्र में, हम एक चर घोषित करते हैं, मूलभूत अंकगणित करते हैं, एक तालिका को मैनिपुलेट करते हैं, और उसकी वस्तुओं के माध्यम से लूप करते हैं।

## गहराई में जानकारी
Lua की हल्के स्वभाव इसके REPL को प्रोटोटाइपिंग के लिए आदर्श बनाता है। यह Lua के आरंभिक 1990 के दशक से ही है, Lisp जैसी पहले की भाषाओं के लिए इंटरैक्टिव शेल्स से प्रेरित है। अन्य भाषाओं में विकल्पों में Ruby के लिए `irb` और Python के लिए `python` शामिल हैं, प्रत्येक के अपने सुविधाओं का सेट होता है। Lua का REPL न्यूनतावादी है; इसलिए, अन्यों में पाए जाने वाले उन्नत सुविधाओं जैसे कि जटिल डिबगिंग टूल्स की कमी हो सकती है। एक मजबूत अनुभव के लिए, ZeroBrane Studio या LuaDist के LuaRocks जैसे औजार बेसिक REPL से अधिक प्रदान करते हैं।

## देखें भी
- [Lua 5.4 संदर्भ मैनुअल - स्वतंत्र Lua इंटरप्रिटर](https://www.lua.org/manual/5.4/manual.html#6)
- [ZeroBrane Studio](https://studio.zerobrane.com/)
- [LuaRocks](https://luarocks.org/)