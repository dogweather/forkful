---
title:                "Fish Shell: स्ट्रिंग्स को मिलाना"
simple_title:         "स्ट्रिंग्स को मिलाना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

# क्यों

क्या आप अपने Fish Shell प्रोग्रामिंग स्किल को बढ़ाना चाहते हैं? तो अपने डेवलपमेंट दौरान आपने कभी स्ट्रिंग्स को कॉनकेटिनेट करने की आवश्यकता महसूस की होगी। इस आर्टिकल में हम आपको बताएंगे कि कैसे फिश शेल बहुत आसानी से स्ट्रिंग्स को कॉनकेटिनेट कर सकता है।

## कैसे

```Fish Shell
set first_name "John"
set last_name "Doe"
set full_name $first_name" "$last_name
echo $full_name
```

आपने देखा कि हमने अपनी पहली और अंतिम नाम को अलग-अलग पैरामीटर्स में स्टोर किया है और फिर उन्हें एक साथ कॉनकेट किया है। इस तरह से फिश शेल आपको स्ट्रिंग्स को आसानी से कॉनकेट करने की सुविधा देता है।

## डीप डाइव

स्ट्रिंग्स को कॉनकेट करने के अलावा, आप उन्हें उम्रित भी बना सकते हैं। इसके लिए आप `string join` कॉमांड का उपयोग कर सकते हैं। इसके अलावा, आप `string split` कॉमांड का उपयोग करके स्ट्रिंग को अलग-अलग भागों में तोड़ सकते हैं।

# आगे देखो

अधिक फिश शेल टिप्स के लिए निम्नलिखित लिंक्स पर जाएं:

- [Fish Shell वेबसाइट](https://fishshell.com/)
- [Fish Shell गाइड](https://fishshell.com/docs/current/)
- [Fish Shell विकि](https://github.com/fish-shell/fish-shell/wiki)