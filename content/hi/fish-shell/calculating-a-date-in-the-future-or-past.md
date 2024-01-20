---
title:                "भविष्य या अतीत की तारीख की गणना"
html_title:           "Fish Shell: भविष्य या अतीत की तारीख की गणना"
simple_title:         "भविष्य या अतीत की तारीख की गणना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
भविष्य और अतीत की तारीख की गणना वह प्रक्रिया है जिसमें हम किसी विशेष तारीख को आगे या पिछे बढ़ाते हैं। प्रोग्रामर्स इसे करते हैं ताकि वे समय-संबंधी लॉजिक को स्वताः ही बदल सकें और सॉफ्टवेयर की कार्यक्षमता को बढ़ा सकें। 

## कैसे करें:
```Fish Shell
#आज की तारीख
set current_date (date +%F)
echo $current_date

#20 दिनों के बाद की तारीख 
set future_date (date -d "$current_date +20 days" +%F)
echo $future_date

#20 दिनों पूर्व की तारीख
set past_date (date -d "$current_date -20 days" +%F)
echo $past_date
```
स्वरूप उत्तीर्ण आउटपुट:
```Fish Shell
2022-04-25
2022-05-15
2022-04-05
```

## गहरा डाइव:
(1) हिस्टोरिकल कॉन्टेक्स्ट: गणना की स्वताः सत्यापन करने कीजिस चुनौती हमेशासे ही प्रोग्रामरों को व्यापारिक और गैर-व्यापारिक परिस्थितियों में मिली है।
(2) विकल्प: यदि आप भाषा को कर्णल आदेशों पर निर्भर नहीं होना चाहते हैं तो आप तारीख वाले मॉड्यूल, Joda-टाइम या Java 8 से आए नए तारीख/समय API जैसे पुस्तकालयों का उपयोग कर सकते हैं।
(3) आवश्यक विवरण: Fish Shell, डेट कमांड के '-d' विकल्प का उपयोग करके भविष्य या अतीत की तारीख की गणना करता है। यहां '+' और '-' माध्यम से हम सप्ताह, दिन, महीने या वर्षों के लिए समय को आगे या पीछे बढ़ा सकते हैं।

## देखें भी:
- [Fish Shell डॉक्युमेंटेशन](https://fishshell.com/docs/current/index.html)
- [Fish Shell कोडिंग संदर्भ](https://fishshell.com/docs/current/commands.html)
- [Date Command in Linux](https://www.geekhideout.com/date.shtml)