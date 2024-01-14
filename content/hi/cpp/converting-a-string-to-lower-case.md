---
title:    "C++: स्ट्रिंग को लोअर केस में रूपांतरण करना"
keywords: ["C++"]
---

{{< edit_this_page >}}

## आप इस काम को क्यों करना चाहते हैं?

कोई सबसे मूलभूत काम है किसी भी प्रोग्रामिंग भाषा में शब्द स्ट्रिंग को मूलशब्द में बदलना। इसलिए, यदि आपको कोई शब्द स्ट्रिंग परिवर्तन करना हो, तो आपको उसे लोअर केस में बदलना होगा। इससे आपकी प्रोग्राम में शब्द स्ट्रिंग का प्रयोग आसान हो जाएगा और समझने में भी आसान होगा।

## कैसे करें?

यह काम सरल है। आप बस नीचे दिए गए कोड द्वारा हिंदी प्रोग्रामिंग में शब्द स्ट्रिंग को लोअर केस में बदल सकते हैं।

```C++
#include <iostream>
#include <string>
#include <algorithm>

using namespace std;

int main() {
  string str;
  cout << "कृपया शब्द स्ट्रिंग डालें: ";
  getline(cin, str);

  // शब्द स्ट्रिंग को लोअर केस में बदलने का तरीका
  transform(str.begin(), str.end(), str.begin(), ::tolower);

  cout << "नया शब्द स्ट्रिंग: " << str;

  return 0;
}
```
### स्क्रीनशॉट:
```
कृपया शब्द स्ट्रिंग डालें: नमस्ते हिंदी भाषा
नया शब्द स्ट्रिंग: नमस्ते हिंदी भाषा
```

## गहराई में जाएं

लोअर केस में शब्द स्ट्रिंग बनाने के लिए, हमने स्ट्रिंग बिल्ड-इन फ़ंक्शन `transform()` का उपयोग किया है। यह फ़ंक्शन उसी स्ट्रिंग में एक नया स्ट्रिंग बनाता है और वही फ़ंक्शन हमें नई स्ट्रिंग मीटर देता है। इस तरह, हमारा मूल शब्द स्ट्रिंग बरकरार रहता है। लोअर केस में शब्द स्ट्रिंग बनाने के लिए, हमने `transform()` फ़ंक्शन के तीसरे सं