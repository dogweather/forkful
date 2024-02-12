---
title:                "HTML विश्लेषण"
aliases: - /hi/fish-shell/parsing-html.md
date:                  2024-02-03T19:13:25.180638-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTML विश्लेषण"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

HTML पार्सिंग, HTML सामग्री से डेटा या जानकारी निकालने के बारे में है, जो कि वेब डेटा से निपटने के दौरान एक सामान्य कार्य है। प्रोग्रामर इसे वेबसाइटों से जानकारी के स्वचालित निष्कर्षण के लिए करते हैं, जैसे कि वेब स्क्रैपिंग, डेटा माइनिंग या स्वचालित परीक्षण के लिए।

## कैसे:

फिश शेल मुख्य रूप से HTML को सीधे पार्स करने के लिए नहीं बनाया गया है। हालाँकि, यह `curl`, `grep`, `sed`, `awk` जैसे Unix औजारों को एक साथ जोड़ने या `pup` या पायथन स्क्रिप्ट में `beautifulsoup` जैसे विशेषज्ञ उपकरणों का उपयोग करने में उत्कृष्ट है। नीचे उदाहरणों में दिखाया गया है कि कैसे फिश शेल के भीतर से इन उपकरणों का लाभ उठाकर HTML को पार्स किया जाए।

### `curl` और `grep` का उपयोग करते हुए:
HTML सामग्री लाना और उन पंक्तियों को निकालना जिसमें लिंक्स होते हैं:

```fish
curl -s https://example.com | grep -oP '(?<=href=")[^"]*'
```

आउटपुट:
```
/page1.html
/page2.html
...
```

### `pup` (HTML पार्सिंग के लिए एक कमांड-लाइन टूल) का उपयोग करते हुए:

पहले, सुनिश्चित करें कि `pup` स्थापित है। फिर आप इसका उपयोग उनके टैग्स, आईडी, क्लासेज, आदि से तत्वों को निकालने के लिए कर सकते हैं।

```fish
curl -s https://example.com | pup 'a attr{href}'
```

आउटपुट, `grep` उदाहरण के समान, `<a>` टैग्स के href अट्रिब्यूट्स को सूचीबद्ध करेगा।

### पायथन स्क्रिप्ट और `beautifulsoup` के साथ:

जबकि फिश स्वयं HTML को मूल रूप से पार्स नहीं कर सकता, यह पायथन स्क्रिप्ट्स के साथ बिना किसी बाधा के एकीकृत करता है। नीचे एक संक्षिप्त उदाहरण है जो पायथन के साथ `BeautifulSoup` का उपयोग करके HTML से शीर्षकों को पार्स और निकालता है। सुनिश्चित करें कि आपके पायथन वातावरण में `beautifulsoup4` और `requests` स्थापित हैं।

**parse_html.fish**

```fish
function parse_html -a url
    python -c "
import sys
import requests
from bs4 import BeautifulSoup

response = requests.get(sys.argv[1])
soup = BeautifulSoup(response.text, 'html.parser')

titles = soup.find_all('title')

for title in titles:
    print(title.get_text())
" $url
end
```

उपयोग:

```fish
parse_html 'https://example.com'
```

आउटपुट:
```
Example Domain
```

ये प्रत्येक विधियाँ विभिन्न उपयोग के मामलों और जटिलता के पैमाने, साधारण कमांड-लाइन टेक्स्ट मैनिपुलेशन से लेकर पायथन स्क्रिप्ट्स में `beautifulsoup` की पूरी पार्सिंग शक्ति तक सेवा प्रदान करती हैं। आपकी आवश्यकताओं और HTML संरचना की जटिलता के आधार पर, आप एक सरल Unix पाइपलाइन या एक अधिक शक्तिशाली स्क्रिप्टिंग दृष्टिकोण चुन सकते हैं।
