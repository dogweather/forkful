---
title:                "C#: स्ट्रिंग को लोअर केस में रूपांतरित करना"
simple_title:         "स्ट्रिंग को लोअर केस में रूपांतरित करना"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्यों
यदि आप coding या programming की दुनिया में हैं, तो आपने स्ट्रिंग को lower case में convert करने के बारे में सुना होगा। यह एक आम काम है जो हमारे code में उपयोग करने के लिए किया जाता है। इस ब्लॉग पोस्ट में हम देखेंगे कि स्ट्रिंग को lower case में convert करने का फायदा क्या है।

## कैसे
अब हम सीखेंगे कि स्ट्रिंग को lower case में convert कैसे करते हैं। हम इस टॉपिक को C# programming language के माध्यम से देखेंगे। निचे दिए गए "```C# ... ```" code blocks में हम दिखाएंगे कि आप कैसे string.ToLower() function का प्रयोग करके स्ट्रिंग को lower case में convert कर सकते हैं। इसके साथ हम उसका output भी दिखाएंगे।

```C#
string str = "HINDI BLOG";
string lowerCaseStr = str.ToLower();
Console.WriteLine(lowerCaseStr); // output: hindi blog
```
[C# string.ToLower() documentation](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower?view=net-5.0)

## गहराई में जाएं
अब, हम इस प्रकार के स्ट्रिंग को lower case में convert करने के बारे में और गहराई से बात करेंगे। हम देखेंगे कि इस function के पीछे क्या logic है और यह कैसे काम करता है। हम इस प्रक्रिया को समझने के लिए स्ट्रिंग के UTF-16 कोड points के साथ खेलेंगे जिससे हम अलग अलग cases की जांच करेंगे और स्ट्रिंग को lower case में convert करेंगे।

स्ट्रिंग को lower case में convert करने का यह process बहुत simple है। हम स्ट्रिंग के हर character को lower case में convert कर देते हैं और फिर उस स्ट्रिंग को return कर देते हैं। हम इस process को फिर से लिखने की जरुरत नहीं होती है क्योंकि C# में string.ToLower() function पहले से ही उपलब्ध होता है जो हमारे लिए यह सभी steps को आसान बनाता है। 

## देखें भी
-