---
title:                "मानक त्रुटि में लिखना"
date:                  2024-01-19
html_title:           "Arduino: मानक त्रुटि में लिखना"
simple_title:         "मानक त्रुटि में लिखना"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/lua/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
Standard error (stderr) एक output stream होता है जो errors और diagnostic messages को display करता है. Programmers इसका इस्तेमाल करते हैं ताकि वे errors को user को सही ढंग से दिखा सकें और log files को clean रख सकें.

## How to: (कैसे करें:)
```Lua
-- Lua में standard error में लिखने का बेसिक उदाहरण
io.stderr:write("यह एक error message है.\n")

-- प्रोग्राम को एक error के साथ terminate करना
os.exit(1)
```

Sample Output:
```
यह एक error message है.
```

## Deep Dive (गहराई में जानकारी)
Lua में, `io.stderr` का इस्तेमाल कर standard error stream में लिखा जा सकता है. यह परंपरा UNIX systems से आई है जहां stdout (standard output), stderr (standard error), और stdin (standard input) मुख्य communication channels हैं. Alternatives में error handling libraries और logging frameworks शामिल हैं, जो advanced features प्रदान करते हैं. Implementing writing to stderr बहुत straightforward है और Lua runtime द्वारा manage किया जाता है.

## See Also (और जानकारी के लिए)
- [Lua 5.4 Reference Manual](https://www.lua.org/manual/5.4/)
- [Programming in Lua (first edition)](https://www.lua.org/pil/contents.html)
- [Lua-users wiki: I/O Library Tutorial](http://lua-users.org/wiki/IoLibraryTutorial)
