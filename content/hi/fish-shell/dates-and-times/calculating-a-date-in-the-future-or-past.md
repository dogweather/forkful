---
title:                "भविष्य या अतीत में तारीख की गणना"
aliases:
- /hi/fish-shell/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-20T17:31:03.180937-07:00
model:                 gpt-4-1106-preview
simple_title:         "भविष्य या अतीत में तारीख की गणना"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

भविष्यता या अतीत की तारीख की गणना करना एक महत्त्वपूर्ण काम है जिसका उपयोग समय-संबंधी लॉजिक सेट करने में होता है। डेवलपर्स इसे कार्यात्मक परियोजनाओं में, जैसे की समय सीमाओं की गणना, रिमाइंडर्स सेट करना और हिस्टोरिकल डेटा एनालिसिस के लिए उपयोग करते हैं।

## कैसे करें:

Fish Shell में डेट को मैनिपुलेट करना सरल है। यहां उदाहरण हैं:

```Fish Shell
# आज से 10 दिन बाद की तारीख प्राप्त करें
set -l future_date (date -d "+10 days" +"%Y-%m-%d")
echo $future_date

# आज से 5 दिन पहले की तारीख प्राप्त करें
set -l past_date (date -d "-5 days" +"%Y-%m-%d")
echo $past_date
```

सैंपल आउटपुट होगा:

```
2023-03-21
2023-03-06
```

## गहराई से जानकारी

Fish Shell में तारीखों की गणना GNU `date` कमांड के साथ की जाती है, जो Unix-Like सिस्टम्स पर मानक टूल है। 

अतीत में, शेल स्क्रिप्ट्स में डेट कैलकुलेशन के लिए बहुत सी जटिलताएँ और कठिनाइयाँ हुआ करती थीं। अब, Fish Shell और `date` का इस्तेमाल करके, यह प्रक्रिया सरल हो गई है।

वैकल्पिक रूप से, अन्य प्रोग्रामिंग भाषाएँ जैसे कि Python या JavaScript इस कार्य के लिए अपने डेट हैंडलिंग फंक्शंस प्रदान करती हैं, लेकिन यदि आपकी पसंद शेल स्क्रिप्टिंग है, तो Fish Shell एक उत्कृष्ट चुनाव है।

Fish Shell का उपयोग करते हुए, शेल स्क्रिप्ट के भीतर जटिल समय गणनाएँ आसानी से और अधिक पठनीय रूप से प्रदर्शित होती हैं। यह बेहतर स्क्रिप्ट मैनेजमेंट और डेबगिंग के लिए उत्साहवर्धक है।

## सम्बंधित स्रोत

- Fish Shell Documentation: https://fishshell.com/docs/current/index.html
- GNU Coreutils (date command): https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- अधिक शेल स्क्रिप्टिंग टिप्स: https://wiki.bash-hackers.org/start
