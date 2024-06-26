---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:13:14.174143-07:00
description: "\u0915\u0948\u0938\u0947: Lua \u092E\u0947\u0902, \u090F\u0915 \u090F\
  \u0938\u094B\u0938\u093F\u090F\u091F\u093F\u0935 \u0910\u0930\u0947 (\u092F\u093E\
  \ \u090F\u0915 \u091F\u0947\u092C\u0932, Lua-\u092D\u093E\u0937\u093E \u092E\u0947\
  \u0902) \u092C\u0928\u093E\u0928\u093E \u0938\u0940\u0927\u093E \u0939\u0948\u0964\
  \ \u0906\u092A \u0938\u093E\u092E\u093E\u0928\u094D\u092F \u0938\u0902\u0916\u094D\
  \u092F\u093E\u0924\u094D\u092E\u0915 \u0907\u0902\u0921\u093F\u0915\u094D\u0938\
  \ \u0915\u094B \u0905\u092A\u0928\u0940 \u092A\u0938\u0902\u0926 \u0915\u0940 \u091A\
  \u093E\u092C\u093F\u092F\u094B\u0902 \u0915\u0947 \u0932\u093F\u090F \u091B\u094B\
  \u0921\u093C \u0926\u0947\u0924\u0947 \u0939\u0948\u0902\u0964 \u0907\u0938\u0947\
  \u2026"
lastmod: '2024-03-13T22:44:52.537218-06:00'
model: gpt-4-0125-preview
summary: "Lua \u092E\u0947\u0902, \u090F\u0915 \u090F\u0938\u094B\u0938\u093F\u090F\
  \u091F\u093F\u0935 \u0910\u0930\u0947 (\u092F\u093E \u090F\u0915 \u091F\u0947\u092C\
  \u0932, Lua-\u092D\u093E\u0937\u093E \u092E\u0947\u0902) \u092C\u0928\u093E\u0928\
  \u093E \u0938\u0940\u0927\u093E \u0939\u0948\u0964 \u0906\u092A \u0938\u093E\u092E\
  \u093E\u0928\u094D\u092F \u0938\u0902\u0916\u094D\u092F\u093E\u0924\u094D\u092E\u0915\
  \ \u0907\u0902\u0921\u093F\u0915\u094D\u0938 \u0915\u094B \u0905\u092A\u0928\u0940\
  \ \u092A\u0938\u0902\u0926 \u0915\u0940 \u091A\u093E\u092C\u093F\u092F\u094B\u0902\
  \ \u0915\u0947 \u0932\u093F\u090F \u091B\u094B\u0921\u093C \u0926\u0947\u0924\u0947\
  \ \u0939\u0948\u0902\u0964 \u0907\u0938\u0947 \u0926\u0947\u0916\u0947\u0902."
title: "\u0938\u0939\u092F\u094B\u0917\u0940 \u0905\u0930\u0947\u091C\u093C \u0915\
  \u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0928\u093E"
weight: 15
---

## कैसे:
Lua में, एक एसोसिएटिव ऐरे (या एक टेबल, Lua-भाषा में) बनाना सीधा है। आप सामान्य संख्यात्मक इंडिक्स को अपनी पसंद की चाबियों के लिए छोड़ देते हैं। इसे देखें:

```Lua
-- एक एसोसिएटिव ऐरे बनाना
userInfo = {
  name = "Jamie",
  occupation = "Adventurer",
  level = 42
}

-- तत्वों तक पहुँच
print(userInfo["name"]) -- Jamie प्रिंट करता है
print(userInfo.occupation) -- Adventurer प्रिंट करता है

-- नयी कुंजी-मान जोड़ी जोड़ना
userInfo["hobby"] = "Coding"
userInfo.favLang = "Lua"

-- एसोसिएटिव ऐरे के ऊपर इटरेट करना
for key, value in pairs(userInfo) do
  print(key .. ": " .. value)
end
```

आउटपुट:
```
Jamie
Adventurer
name: Jamie
occupation: Adventurer
level: 42
hobby: Coding
favLang: Lua
```

मजेदार हिस्सा? आप डेटा के साथ उन चाबियों का उपयोग करके इंटरैक्ट करते हैं जो आपके लिए मायने रखती हैं, कोड को और अधिक पठनीय और रख-रखाव योग्य बनाती हैं।

## गहराई में
जब Lua दृश्य पर आया, तो इसने टेबल्स को एक सर्व-उद्देश्यीय डेटा संरचना के रूप में पेश किया, जिससे डेवलपर्स डेटा का प्रबंधन कैसे करते हैं, इसमें क्रांति आयी। कुछ भाषाओं के विपरीत जहां एसोसिएटिव ऐरे और ऐरे विशिष्ट इकाइयाँ होती हैं, Lua की टेबल्स दोनों के रूप में कार्य करती हैं, डेटा संरचना परिदृश्य को सरल बनाती हैं।

Lua टेबल्स को विशेष रूप से शक्तिशाली बनाती है उनकी लचीलापन। हालांकि, इस लचीलापन की कीमत पर आती है, विशेष रूप से बड़े डेटासेट्स के साथ, जहाँ एक अधिक विशेषीकृत डेटा संरचना क्षमता के लिए पसंदीदा हो सकती है।

हालाँकि Lua नेटिव तौर पर अधिक पारंपरिक डेटा संरचनाओं का, जैसे कि लिंक्ड लिस्ट्स या हैश मैप्स, समर्थन नहीं करता, टेबल संरचना की अनुकूलनशीलता का मतलब है कि आप इन्हें टेबल्स का उपयोग करके लागू कर सकते हैं यदि आवश्यक हो। बस याद रखें: महान शक्ति के साथ महान जिम्मेदारी आती है। अपने कोड की क्षमता और पठनीयता को बनाए रखने के लिए लचीलापन का बुद्धिमानी से उपयोग करें।
