---
title:                "दो तारीखों की तुलना"
date:                  2024-01-20T17:32:40.542289-07:00
model:                 gpt-4-1106-preview
simple_title:         "दो तारीखों की तुलना"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
तारीखें तुलना का मतलब है दो तारीखों को आपस में मिलाना। प्रोग्रामर्स इसे करते हैं क्योंकि उन्हें कई बार चेक करना पड़ता है कि किस तारीख से कोई फाइल, इवेंट, या डेटा कितना पुराना या नया है।

## How to: (कैसे करें:)
```Bash
# तारीखें स्ट्रिंग्स के रूप में
date1="2023-03-01"
date2="2023-03-10"

# उन्हें सेकंड्स में कन्वर्ट करें
sec1=$(date -d "$date1" +%s)
sec2=$(date -d "$date2" +%s)

# तुलना करें
if [ $sec1 -eq $sec2 ]; then
    echo "तारीखें बराबर हैं।"
elif [ $sec1 -lt $sec2 ]; then
    echo "$date1 $date2 से पहले की है।"
else
    echo "$date1 $date2 के बाद की है।"
fi
```
सैंपल आउटपुट:
```
2023-03-01 2023-03-10 से पहले की है।
```

## Deep Dive (गहराई में जानकारी):
तारीखों की तुलना UNIX और LINUX सिस्टम में बहुत पुरानी बात है, `date` कमांड का इस्तेमाल 1970 से हो रहा है। `date` कमांड से हम सेकंड्स में तारीखों को बदलते हैं, क्योंकि यह UNIX एपॉक टाइम स्टैंडर्ड का इस्तेमाल करता है। इसके अलावा, `dateutils`, `GNU coreutils` जैसे टूल्स भी तारीखों की तुलना में मदद करते हैं। बैश में, हम ऑपरेटर्स `-eq`, `-lt`, और `-gt` का इस्तेमाल करके सीधे सेकंड्स में कन्वर्टेड तारीखों की तुलना कर सकते हैं।

## See Also (और भी देखें):
- बैश मैन पेज: https://www.gnu.org/software/bash/manual/
- GNU coreutils: https://www.gnu.org/software/coreutils/
- dateutils डॉक्यूमेंटेशन: http://www.fresse.org/dateutils/