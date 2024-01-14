---
title:                "Python: एक पैटर्न के समतुलित अक्षरों को हटाना"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## क्यों

कभी-कभी हमें एक पुराने पैटर्न या वर्णक्रम को हटाने की आवश्यकता होती है। इसलिए, हम एक पैटर्न मेचिंग फ़ंक्शन का उपयोग करके वहाँ सभी समान वर्ण को हटा सकते हैं।

## कैसे करें

```Python
text = "इस रवि दिन आकाश साफ़ है।"
pattern = "ा"
cleaned_text = text.replace(pattern, "")
print(cleaned_text)
```

आउटपुट: "इस रवि दिन क् साफ़ है।"

```Python
text = "लोकप्रियता के बारे में अधिक सूचना के लिए, देखें हमारी ब्लॉग पोस्ट"
pattern = "े"
cleaned_text = text.replace(pattern, "")
print(cleaned_text)
```

आउटपुट: "लोकप्रियता क बार म अधिक सूचन क लिए, देख हमार ब्लॉग पोस्ट"

## गहराई से जानें

पैटर्न मैचिंग फ़ंक्शन समान वर्णों को हटा सकता है, जो आपको दोनों वाक्यों में भिन्नता देता है। इस तरह से, आप आसानी से वाक्य या शब्द में अलग-अलग वर्णों को हटा सकते हैं। यह आपको पाठ्यपुस्तकों और आर्टिकलों की सुविधा प्रदान करता है।

## देखें

[Python में पैटर्न मैचिंग कैसे करें](https://www.w3schools.com/python/python_regex.asp)

[Python डॉक्यूमेंटेशन: स्ट्रिंग विधियाँ](https://docs.python.org/3/library/stdtypes.html#string-methods)

[बोलबद्ध गाइड: मैनुअल की जांच](https://realpython.com/python-regex/#checking-for-aPattern)