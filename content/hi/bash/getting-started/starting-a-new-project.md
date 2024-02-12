---
title:                "नई परियोजना शुरू करना"
aliases:
- /hi/bash/starting-a-new-project/
date:                  2024-01-20T18:03:40.334862-07:00
model:                 gpt-4-1106-preview
simple_title:         "नई परियोजना शुरू करना"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
प्रोजेक्ट की शुरुआत क्या होती है? यह नई सॉफ्टवेयर योजना का प्रारंभिक बिंदु होता है। कोडर्स नई प्रोजेक्ट्स क्यों बनाते हैं? सरल है: नए आइडियाज को साकार करने, समस्याओं का हल निकालने और तकनीकी क्षेत्र में नवाचार के लिए।

## How to: (कैसे करें:)
मान लें कि हम एक नया प्रोजेक्ट बना रहे हैं जिसका नाम `my_project` है। बश में, हमें कुछ फोल्डर्स और फाइल्स बनाने होंगे। सरल कमांड्स से शुरुआत करते हैं:

```Bash
mkdir my_project          # नया फोल्डर बनाएं
cd my_project             # उस फोल्डर में जाएँ
touch README.md           # डॉक्यूमेंटेशन के लिए README फाइल
mkdir src                 # सोर्स कोड के लिए एक फोल्डर
touch src/main.sh         # मुख्य प्रोग्राम फाइल
```

आउटपुट नहीं होता, लेकिन फोल्डर और फाइलें बन जाएंगी।

## Deep Dive (गहन डाइव)
पुराने दिनों में, प्रोग्रामर्स कोड लिखने से पहले बहुत तैयारी करते थे। वे डायग्राम्स, डॉक्यूमेंटेशन बनाते और सम्पूर्ण डिज़ाइन सोचते थे। आजकल, कमांड लाइन टूल्स और विकास फ्रेमवर्क, जैसे कि Git, Docker, और अन्य शॉर्टकट्स, ने प्रोजेक्ट की शुरुआत को काफी आसान बना दिया है। वैकल्पिक विधियों में आधुनिक IDEs जैसे कि Visual Studio Code या JetBrains की प्रोडक्ट्स का इस्तेमाल शामिल है जो प्रोजेक्ट टेम्प्लेट्स को सपोर्ट करते हैं। विस्तृत उपयोग और प्रोजेक्ट की रचना की बारीकियां इसके जटिलता को दर्शाती हैं।

## See Also (यह भी देखें)
- [Bash Scripting Tutorial](https://linuxconfig.org/bash-scripting-tutorial)
- [Learn Shell Programming](https://www.learnshell.org/)
- [Git Basics for Version Control](https://git-scm.com/book/en/v2/Getting-Started-Git-Basics)
- [Docker for Beginners](https://docker-curriculum.com/)
