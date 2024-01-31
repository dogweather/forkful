---
title:                "वर्तमान तारीख प्राप्त करना"
date:                  2024-01-20T15:15:18.023913-07:00
simple_title:         "वर्तमान तारीख प्राप्त करना"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)

करेंट डेट पाना मतलब है आज की तारीख जानना। प्रोग्रामर्स इसे लॉगिंग, टाइमस्टैम्प्स और कार्यक्रमों में समय-आधारित कार्यों के लिए इस्तेमाल करते हैं। 

## कैसे करें? (How to:)

Fish Shell में करेंट डेट पाना बेहद सरल है:

```Fish Shell
# आज की तारीख डिफॉल्ट फॉर्मेट में प्राप्त करें
set -l current_date (date)
echo $current_date

# सैम्पल आउटपुट:
# Wed Mar 31 10:26:58 IST 2023
```

आप अलग-अलग फॉर्मेट में भी तारीख पा सकते हैं:

```Fish Shell
# कस्टम फॉर्मेट में तारीख प्राप्त करें
set -l formatted_date (date "+%Y-%m-%d")
echo $formatted_date

# सैम्पल आउटपुट:
# 2023-03-31
```

## गहराई से जानकारी (Deep Dive)

करेंट डेट पाने के लिए `date` कमांड UNIX लाइन से आयी है और ये बरसों से सिस्टम प्रोग्रामिंग का हिस्सा है। Fish Shell, जो कि एक मॉडर्न शेल है, इसी `date` कमांड को आसान और पर्सनलाइज़ेबल ढंग से यूज़ करता है। 

`date` के विकल्प के रूप में हम `strftime` जैसे बिल्ट-इन फ़ंक्शंस का इस्तेमाल कर सकते हैं, जो कि Fish Shell में उपलब्ध हैं। इसके अलावा, किसी भी फॉर्मेट में तारीख पाने के लिए `date` कमांड के साथ `+"%Y-%m-%d"` जैसे फॉर्मेट स्ट्रिंग्स का उपयोग किया जाता है। 

जब आप अपने Fish Shell स्क्रिप्ट में तारीख से जुड़े ऑपरेशन्स करते हैं, तो आपको सिस्टम के टाइमज़ोन सेटिंग्स का भी ध्यान रखना पड़ता है, क्योंकि ये आउटपुट को प्रभावित करता है।

## संबंधित स्रोत (See Also)

- आधिकारिक Fish Shell डॉक्यूमेंटेशन: [Fish Documentation](https://fishshell.com/docs/current/index.html)
- `date` कमांड के बारे में और इन्फॉर्मेशन: [GNU coreutils date](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
