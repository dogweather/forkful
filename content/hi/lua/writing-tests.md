---
title:                "परीक्षण लिखना"
date:                  2024-01-19
simple_title:         "परीक्षण लिखना"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/lua/writing-tests.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

परीक्षण लिखना सॉफ़्टवेयर की जांचने की एक प्रक्रिया है, जिसमें कोड की मुख्य कार्यक्षमता की पुष्टि की जाती है। प्रोग्रामर इसे बग्स को खोजने और सॉफ़्टवेयर की गुणवत्ता को सुनिश्चित करने के लिए करते हैं।

## कैसे करें:

Lua में एक साधारण टेस्ट फ़ंक्शन का उदाहरण:

```Lua
function add(a, b)
    return a + b
end

function testAddFunction()
    local result = add(5, 3)
    assert(result == 8, "Expected 5 + 3 to equal 8")
end

testAddFunction()
print("Test passed!")
```

ऊपर कोड में, `testAddFunction` कोई गलती को खोजने के लिए `add` फ़ंक्शन का परीक्षण करता है। अगर कोई गलती ना हो, तो आउटपुट होगा:

```
Test passed!
```

## गहराई में जानकारी:

टेस्टिंग का इतिहास सॉफ़्टवेयर के विकास जितना पुराना है। बेशक, प्रकार और तरीके बदले हैं, लेकिन मुख्य उद्देश्य वही है - कोड की त्रुटियों को खोजना। Lua में परीक्षण लिखने के लिए विकल्पों में `busted` और `luassert` शामिल हैं, जो उच्च-स्तरीय टेस्टिंग प्रदान करते हैं। विस्तृत टेस्टिंग के लिए, मॉक ऑब्जेक्ट्स और टेस्ट सूट बनाये जाते हैं।

## देखें भी:

- Lua डॉक्युमेंटेशन: https://www.lua.org/docs.html
- Busted (Lua परीक्षण फ्रेमवर्क): https://olivinelabs.com/busted/
- luassert (Lua एसर्शन्स लाइब्रेरी): https://github.com/Olivine-Labs/luassert

ये स्रोत और जानकारी प्रदान करेंगे और आपको Lua में टेस्टिंग संबंधित बेहतर समझ देंगे।
