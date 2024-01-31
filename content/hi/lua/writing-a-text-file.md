---
title:                "टेक्स्ट फाइल लिखना"
date:                  2024-01-19
html_title:           "Bash: टेक्स्ट फाइल लिखना"
simple_title:         "टेक्स्ट फाइल लिखना"

category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/lua/writing-a-text-file.md"
---

{{< edit_this_page >}}

## क्या और क्यों? 
टेक्स्ट फ़ाइल लिखना, यानी डाटा को टेक्स्ट फॉर्म में फ़ाइल में सेव करना। प्रोग्रामर्स ये इसीलिए करते हैं क्योंकि ये डाटा को बचाने, शेयर करने और बाद में पढ़ने का एक सरल तरीका है।

## कैसे करें:
```Lua
local file_path = "example.txt"
local file_content = "नमस्ते दुनिया!"

-- नई फ़ाइल खोलें और लिखें
local file = io.open(file_path, "w")
file:write(file_content)
file:close()
```
सैंपल आउटपुट: `example.txt` फ़ाइल में "नमस्ते दुनिया!" सेव हो जाएगा।

## गहराई में:
लुआ (Lua) में टेक्स्ट फ़ाइल लिखने की क्षमता मूल रूप से इनबिल्ट है। इतिहास में देखा जाए, तो यह सुविधा प्रोग्रामिंग के शुरुआती दिनों से ही महत्वपूर्ण रही है। अल्टरनेटिव्स में डाटाबेस या बायनरी फ़ाइल सिस्टम्स शामिल हैं। फ़ाइल आई/ओ (I/O) ऑपरेशंस सिस्टम पर निर्भर करते हैं, लेकिन लुआ इन्हें एक सहज इंटरफ़ेस के ज़रिए संभव बनाता है।

## संबंधित सूत्र:
- [Lua 5.4 Reference Manual for I/O](https://www.lua.org/manual/5.4/manual.html#6.8)
- [Lua File Tutorial on TutorialsPoint](https://www.tutorialspoint.com/lua/lua_file_io.htm)
