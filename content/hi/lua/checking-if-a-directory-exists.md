---
title:                "एक निर्देशिका मौजूद है या नहीं जांचना"
html_title:           "Lua: एक निर्देशिका मौजूद है या नहीं जांचना"
simple_title:         "एक निर्देशिका मौजूद है या नहीं जांचना"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/lua/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

डायरेक्टरी मौजूद हैं की जांच करना हमें ये समझने में मदद करता है कि वह वास्तविक रूप से उपस्थित हैं या नहीं। प्रोग्रामर्स इसे ऐसी एरर की रोकथाम के लिए करते हैं जो तब होती है जब वे एक अगर इसे अस्तित्व में न होने पर इस्तेमाल करने की कोशिश करें।

## कैसे:

Lua में, आप ```lfs``` (Lua file system) मॉड्यूल का उपयोग करके डायरेक्टरी की मौजूदगी की जाँच कर सकते हैं, कैसे करें:

```Lua
lfs = require('lfs')

if lfs.attributes('your_directory_path', 'mode') == 'directory' then
    print('Directory exists')
else
    print('Directory does not exist')
end
```
यदि डायरेक्टरी मौजूद है, इसमें 'Directory exists' प्रिंट होगा, अन्यथा 'Directory does not exist' प्रिंट होगा।

## गहराई में:

Lua में फ़ाइल सिस्टम का कामकाज करने के लिए "lfs" मॉड्यूल 2003 में पेश किया गया था। "lfs.attributes()" फ़ंक्शन का उपयोग करके डायरेक्टरी या फ़ाइल के विशेषताओं का पता लगाया जा सकता है। 'mode' एट्रिब्यूट 'directory' का होना इसका संकेत देता है कि पथ एक डायरेक्टरी है।

'os.execute' जैसे विकल्प भी हैं, लेकिन वे "lfs" की तुलना में तकनीकी और असुरक्षित हैं।

यदि फ़ंक्शन 'nil' को लौटता है, तो यह इसका संकेत देता है कि डायरेक्टरी मौजूद नहीं है।

## अन्य स्रोतों को देखें:

डायरेक्टरी की जाँच के अन्य विचारधाराओं और विकल्पों के बारे में और पढ़ने के लिए, निम्नलिखित लिंक्स देखें:
- [Lua User's Wiki: File System Operations](http://lua-users.org/wiki/FileSystemOperations)
- [Lua FileSystem on GitHub](https://github.com/keplerproject/luafilesystem)