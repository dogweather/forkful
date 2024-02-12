---
title:                "दो तारीखों की तुलना"
date:                  2024-01-20T17:33:19.187659-07:00
model:                 gpt-4-1106-preview
simple_title:         "दो तारीखों की तुलना"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
तारीखों की तुलना का मतलब है दो डेट्स को आपस में मिलाना। प्रोग्रामर अक्सर इसका इस्तेमाल करते हैं टाइमलाइन्स मैनेज करने, सॉर्टिंग के लिए, या डेडलाइंस चेक करने में।

## How to: (कैसे करें)
Fish Shell में दो तारीखों की तुलना करने के लिए हमें उन्हें UNIX टाइमस्टैम्प में बदलना होगा। यहाँ एक सिंपल मेथड है:

```Fish Shell
# तारीख 1 और तारीख 2 सेकंड्स में बदलें
set date1 (date -ud '2023-01-01' +%s)
set date2 (date -ud '2023-03-01' +%s)

# तुलना करें
if test $date1 -lt $date2
    echo "तारीख 1 पहले है तारीख 2 से।"
else if test $date1 -eq $date2
    echo "दोनों तारीखें बराबर हैं।"
else
    echo "तारीख 2 पहले है तारीख 1 से।"
end
```
आउटपुट:
```
तारीख 1 पहले है तारीख 2 से।
```

## Deep Dive (गहराई में जानकारी)
Fish Shell के पूर्ववर्ती वर्शन्स में भी तारीखों की तुलना कुछ इसी प्रकार से होती थी, लेकिन नए वर्शन्स में इसे और भी सरल कर दिया गया है। वैकल्पिक तरीके में `date` कमांड की बजाय `strftime` जैसे फंक्शंस का इस्तेमाल होता है। कमांड लाइन टूल्स के अलावा, दूसरे लैंग्वेजेज जैसे कि Python या JavaScript में इनबिल्ट फंक्शन्स होते हैं डेट कम्पेरिज़न के लिए। Fish Shell में प्राइमरिली यूनिक्स टाइमस्टैम्प्स पर आधारित मेथड्स उपयोग किए जाते हैं।

## See Also (और भी जानकारी)
- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [GNU Coreutils 'date'](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [UNIX Timestamp Converter](https://www.unixtimestamp.com/)
