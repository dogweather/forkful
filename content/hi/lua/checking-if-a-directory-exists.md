---
title:                "डायरेक्टरी का अस्तित्व जाँचना"
date:                  2024-01-20T14:58:03.351112-07:00
html_title:           "Elm: डायरेक्टरी का अस्तित्व जाँचना"
simple_title:         "डायरेक्टरी का अस्तित्व जाँचना"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/lua/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
डायरेक्टरी का अस्तित्व चेक करना मतलब है पता करना कि एक फोल्डर सिस्टम में है भी या नहीं। प्रोग्रामर इसे इसलिए करते हैं कि फाइल ऑपरेशन्स बिना एरर के चलें।

## कैसे करें? (How to:)
```Lua
local lfs = require("lfs")

function directory_exists(path)
  local ok, err, code = lfs.attributes(path, "mode")
  return ok and code == 13 or false, err
end

-- उदाहरण
local dirPath = "/path/to/directory"

if directory_exists(dirPath) then
  print("डायरेक्टरी मौजूद है!")
else
  print("डायरेक्टरी मौजूद नहीं है।")
end
```

## गहराई में (Deep Dive)
Lua में डायरेक्टरी का अस्तित्व जांचने के लिए `lfs` (Lua File System) मॉड्यूल का इस्तेमाल होता है। यह फ़ंक्शनलिटी लुआ के कोर में नहीं है, इसलिए `lfs` को पहले इंस्टॉल करना पड़ता है। `lfs.attributes` को कॉल करके हमें एरर कोड मिलता है, जिसे हमें हैंडल करना पड़ सकता है। हिस्टोरिकल कॉन्टेक्स्ट में, यह एप्रोच बाकी प्रोग्रामिंग भाषाओं के जैसा ही है। विकल्पों में `os` के `execute` फ़ंक्शन का इस्तेमाल करना शामिल है, जो सिस्टम कमांड्स चलाता है, लेकिन यह तरीका प्लेटफ़ोर्म पर निर्भर करता है।

## संबंधित स्रोत (See Also)
- [Programming in Lua (book)](http://www.lua.org/pil/)
- [Lua 5.3 Reference Manual](https://www.lua.org/manual/5.3/)