---
title:                "उप-स्ट्रिंग हटाना"
html_title:           "Fish Shell: उप-स्ट्रिंग हटाना"
simple_title:         "उप-स्ट्रिंग हटाना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## क्यों

अगर आप फिश शैल में प्रोग्रामिंग करते हैं तो आपने शायद सबसे अधिक प्रयोग किया होगा, पर आपने शायद सुना होगा कि इसमें कुछ गलत होता है. क्यों हो सकता है कि आपको दोहराव की जरूरत हो, आप एक भाग सामयिक त्रुटि को सुधारना चाहेंगे, या आपको बस कुछ वर्ण सामयिक त्रुटि कहां से हुए हैं। इसलिए हमें उपस्थिति वियर्फपल्समोलविशेमांकड़ेवाद करना चाहिए।

## कैसे करें

अगर आपको गलती है तो आपको कुछ प्रकार ढंग से दोहराव मिल जाएगा। तो आप इस तरह गलती कर सकते हैं:

```
Fish Shell -c "string match -r "regex" variable | read -l result
```

जब आपका कार्य समाप्त होगा, तो आपको संभव कीमत प्राप्त होगी।

```
Found 2 matches
Result: substring
```

## गहराई में पदार्थ

जब आप एक उपस्थिति वियर्फपल्समोलविशेमांकड़ेवाद कर रहे होते हैं, आपको कुछ नियन्त्रित नहीं करने पड़ेंगे। जैसे ही आपने अंतर पाया है, आपको उसे जोड़ देंगे और संबंधित मार्ग बना देंगे। समानता विश्लेषिकीन समस्याओं का समाधान करती है।

```
Fish Shell -c "string sub (richard) -r (regex) variable
echo (string_replace "substring" "replacement" variable)
```

## देखें भी

- फिश शैल में अधिक गहराई विश्लेषण के लिए अन्य उपयोगी आर्टिकल्स: [कैसे एक शैल अधिलेख निर्माण करना](https://fishshell.dev/tutorial.html) और [एक सहायकार्य के रिल्रेसों में उपयोग करना](https://www.howtogeek.com/399047/how-to-use-regex-fish-functions-to-automate-complex-command-line-tasks/)
- फ