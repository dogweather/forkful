---
title:    "Fish Shell: स्ट्रिंग को कैपिटलाइज़ करना"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## क्यों

एक स्ट्रिंग को कैपिटलाइज करने का कारण स्ट्रिंग को अपशब्दों और गुणांकों में विभाजित करना है, जो कोड को स्पष्ट बनाता है और कॉन्सिस्टेंटी बनाता है।

## कैसे करें

```Fish Shell कोड उदाहरण
set my_string "hello world"
echo $my_string | string to-caps
```

### आउटपुट:

```Hindi
HELLO WORLD
```

## डीप डाइव

स्ट्रिंग फ़ंक्शन स्ट्रिंग को प्रिंट, मुद्रित करने और सामान्य ओटियन में विभाजित करते हैं। string to-caps फ़ंक्शन को डिफ़ॉल्ट ऑप्शन के रूप में उपयोग किया जाना सुनिश्चित करेगा, जो स्ट्रिंग को ऊंचे केस में बदल देगा। आप सभी कोडिंग ओप्शन के बारे में अधिक जानने के लिए, "man" में मोड दिया गया है।

## देखिये भी

[Fish Shell की दस्तावेज़ीकरण] (https://fishshell.com/docs/current/index.html)

[स्ट्रिंग फ़ंक्शन का उपयोग करें] (https://fishshell.com/docs/current/cmds/string.html)

[फिश शेल समुदाय] (https://fishshell.com/docs/current/guides.html)