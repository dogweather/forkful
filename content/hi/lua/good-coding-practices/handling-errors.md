---
date: 2024-01-26 00:56:38.695106-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: Lua \u092E\u0947\u0902\
  \ \u0926\u094B \u092E\u0941\u0916\u094D\u092F \u092B\u0902\u0915\u094D\u0936\u0902\
  \u0938 \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0924\u094D\u0930\u0941\u091F\
  \u093F \u0938\u0902\u092D\u093E\u0932\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F\
  \ \u0915\u093F\u092F\u093E \u091C\u093E\u0924\u093E \u0939\u0948: `pcall` \u0914\
  \u0930 `xpcall`\u0964 \u092F\u0939\u093E\u0902 \u092C\u0924\u093E\u092F\u093E \u0917\
  \u092F\u093E \u0939\u0948 \u0915\u093F \u0906\u092A \u0909\u0928\u0915\u093E \u0909\
  \u092A\u092F\u094B\u0917 \u0915\u0948\u0938\u0947 \u0915\u0930 \u0938\u0915\u0924\
  \u0947 \u0939\u0948\u0902."
lastmod: '2024-03-13T22:44:52.562561-06:00'
model: gpt-4-1106-preview
summary: "Lua \u092E\u0947\u0902 \u0926\u094B \u092E\u0941\u0916\u094D\u092F \u092B\
  \u0902\u0915\u094D\u0936\u0902\u0938 \u0915\u093E \u0909\u092A\u092F\u094B\u0917\
  \ \u0924\u094D\u0930\u0941\u091F\u093F \u0938\u0902\u092D\u093E\u0932\u0928\u0947\
  \ \u0915\u0947 \u0932\u093F\u090F \u0915\u093F\u092F\u093E \u091C\u093E\u0924\u093E\
  \ \u0939\u0948."
title: "\u090F\u0930\u0930\u094D\u0938 \u0915\u094B \u0939\u0948\u0902\u0921\u0932\
  \ \u0915\u0930\u0928\u093E"
weight: 16
---

## कैसे करें:
Lua में दो मुख्य फंक्शंस का उपयोग त्रुटि संभालने के लिए किया जाता है: `pcall` और `xpcall`। यहां बताया गया है कि आप उनका उपयोग कैसे कर सकते हैं:

```lua
function might_fail()
    if math.random() > 0.5 then
        error("ओहो! कुछ गलत हो गया।")
    else
        print("सब ठीक है!")
    end
end

-- pcall का उपयोग करते हुए
local success, errorMessage = pcall(might_fail)

if success then
    print("सफलता!")
else
    print("त्रुटि को पकड़ा गया:", errorMessage)
end

-- xpcall का उपयोग एक त्रुटि हैंडलर के साथ करते हुए
function myErrorHandler(err)
    print("त्रुटि हैंडलर कहता है:", err)
end

local status = xpcall(might_fail, myErrorHandler)
print("क्या कॉल सफल रही?", status)
```

नमूना आउटपुट हो सकता है:

```
त्रुटि को पकड़ा गया: ओहो! कुछ गलत हो गया।
त्रुटि हैंडलर कहता है: ओहो! कुछ गलत हो गया।
क्या कॉल सफल रही? नहीं
```
या, अगर कोई त्रुटि नहीं होती है:
```
सब ठीक है!
सफलता!
सब ठीक है!
क्या कॉल सफल रही? हां
```

## गहराई से विचार
त्रुटियों को संभालना, या "अपवाद संभालना," हमेशा से नहीं था। प्रारंभिक प्रोग्राम अक्सर क्रैश हो जाते थे। कोडिंग के विकास के साथ, स्थिरता की जरूरत भी बढ़ी। Lua का तरीका कुछ भाषाओं की तुलना में सरल है। कोई `try/catch` ब्लॉक नहीं हैं, बस `pcall` और `xpcall` हैं। पहला एक फंक्शन कॉल की रक्षा करता है, वापस स्थिति और किसी भी त्रुटि को लौटा देता है। दूसरा एक त्रुटि हैंडलिंग फंक्शन जोड़ता है, जो व्यक्तिगत सफाई या लॉगिंग के लिए उपयोगी है।

Lua में एक वैकल्पिक तरीका `assert` का उपयोग करना है, जो एक समान उद्देश्य की सेवा कर सकता है, यदि इसकी शर्त गलत है तो एक त्रुटि फेंककर। लेकिन जटिल त्रुटि संभालने के परिदृश्यों के लिए यह `pcall` जितना लचीला नहीं है।

आंतरिक रूप से, `pcall` और `xpcall` "सुरक्षित वातावरण" स्थापित करके काम करते हैं ताकि फंक्शन चल सके। यदि एक त्रुटि पॉप अप होती है, तो वातावरण इसे पकड़ लेता है और इसे तुरंत संभाल सकता है या प्रोग्राम को वापस संभालने के लिए पास कर सकता है।

## यह भी देखें
- 'Programming in Lua' पुस्तक (तीसरा संस्करण), जो https://www.lua.org/pil/ पर उपलब्ध है त्रुटि संभालने पर गहन अध्ययन के लिए (अनुभाग 8.4)।
- आधिकारिक Lua 5.4 संदर्भ मैन्युअल: https://www.lua.org/manual/5.4/ - Lua के त्रुटि संभालने के कार्यों पर सबसे अद्यतन जानकारी के लिए।
- Lua-उपयोगकर्ता विकी पर त्रुटि संभालना: http://lua-users.org/wiki/ErrorHandling – समुदाय की अंतर्दृष्टि और पैटर्न के लिए।
