---
title:                "यादृच्छिक संख्याओं का निर्माण"
html_title:           "Clojure: यादृच्छिक संख्याओं का निर्माण"
simple_title:         "यादृच्छिक संख्याओं का निर्माण"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

# बैश द्वारा यादृच्छिक संख्याओं का उत्पादन कैसे करें 

## क्या और क्यों?
यादृच्छिक संख्याएं उत्पन्न करना मतलब होता है किसी बिना पूर्वनिर्धारित पैटर्न के संख्या को प्राप्त करना। प्रोग्रामर्स इसे हमेशा अनुप्रेक्षिकता बनाए रखने, या परीक्षण और सिमुलेशन के लिए करते हैं।

## कैसे करें:
बाश में एक यादृच्छिक संख्या उत्पन्न करने के लिए आप `$RANDOM` वैरिएबल का उपयोग कर सकते हैं।

```Bash 
echo $RANDOM
```

आउटपुट कुछ इस तरह हो सकता है:

```Bash 
21523
```

## गहन अध्ययन:
`$RANDOM` वैरिएबल का उपयोग करके यादृच्छिक संख्याएं उत्पन्न करना बाश का एक बुनियादी तरीका है। हालांकि, इसे POSIX आधारित शेलों में नहीं पाया जाता है, और इस मामले में आपको `/dev/urandom` फाइल का उपयोग करने की जरूरत हो सकती है। इसका उपयोग `$RANDOM` के बजाय उच्चतर क्वालिटी की यादृच्छिकता के लिए किया जाता है।

## अधिक जानकारी के लिए:
- [बाश मैन पेज](https://man7.org/linux/man-pages/man1/bash.1.html) में `$RANDOM` वारियबल का विवरण
- [$RANDOM पर स्टैक ओवरफ्लो पोस्ट](https://stackoverflow.com/questions/8983163/how-can-i-generate-random-numbers-in-bash) 
- [`/dev/urandom` और `/dev/random` का उपयोग](https://security.stackexchange.com/questions/3936/is-a-rand-from-dev-urandom-secure-for-a-login-key) 
- [एक क्रिप्टोग्राफिकली सुरक्षित प्राप्ति](https://www.2uo.de/myths-about-urandom/) इस के बारे में अधिक विवरण।