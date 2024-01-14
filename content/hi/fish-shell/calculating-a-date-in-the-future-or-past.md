---
title:                "Fish Shell: भविष्य या भूतकाल में तारीख की गणना"
simple_title:         "भविष्य या भूतकाल में तारीख की गणना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्यों
किसी व्यक्ति को भविष्य या भूतकाल में एक दिन की तारीख की गणना करने में हस्तक्षेप करने की आवश्यकता हो सकती है।

## कैसे करें
```Fish Shell```, आप किसी भी भूतकाल या भविष्य में एक दिन की गणना कैसे कर सकते हैं इसके लिए निम्नलिखित कोड ब्लॉक का पालन करें:

### भूतकाल में तारीख की गणना
```Fish Shell
# भूतकाल से 14 दिन पहले की गणना करें
set -l date (date -v -14d +"%d/%m/%Y") 
# परिणाम: 05/11/2021
```

### भविष्य में तारीख की गणना
```Fish Shell
# भविष्य में 1 सप्ताह बाद की गणना करें
set -l date (date -v +1w +"%d/%m/%Y") 
# परिणाम: 29/10/2021
```

## गहराई में जाएं
भूतकाल या भविष्य में एक दिन की गणना करने के लिए आप ```Fish Shell``` के साथ कई अन्य ऑप्शन भी इस्तेमाल कर सकते हैं। आप दिन/महीने/वर्ष के अलावा इस्तेमाल किए जाने वाले अन्य अक्षरों के साथ भी गणना कर सकते हैं। अधिक जानने के लिए आप फिश शेल के दस्तावेज़ के साथ संपर्क कर सकते हैं।

## देखें भी
- [Official Fish Shell documentation](https://fishshell.com/docs/current/index.html)
- [Introduction to Fish Shell](https://www.freecodecamp.org/news/an-introduction-to-the-fish-shell/)

आशा करता हूँ कि आपको यह आलेख ```Fish Shell``` में भविष्य या भूतकाल की तारीख की गणना करने के बारे में कुछ समझ में आया होगा।