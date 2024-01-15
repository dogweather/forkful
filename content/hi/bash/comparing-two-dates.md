---
title:                "दो तारीखों की तुलना करना"
html_title:           "Bash: दो तारीखों की तुलना करना"
simple_title:         "दो तारीखों की तुलना करना"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## क्यों

दो तिथियों को तुलना करने का काम क्यों किया जाता है यह पता करना आपके लिए उपयोगी हो सकता है।

## कैसे करें

दो तिथियों को तुलना करने के लिए आप बैश के उपयोग का उदाहरण देख सकते हैं। 

उदाहरण:

अगर हमें 15 अगस्त, 2021 और 1 सितंबर, 2021 को दो तारीखों की तुलना करनी हो, तो हम निम्नलिखित कोड का उपयोग कर सकते हैं:

```Bash
date1="2021-08-15"
date2="2021-09-01"

if [[ "$date1" > "$date2" ]]; then
  echo "$date1 is greater than $date2"
elif [[ "$date2" > "$date1" ]]; then
  echo "$date2 is greater than $date1"
else
  echo "$date1 and $date2 are equal"
fi

```

आउटपुट:

2021-09-01 is greater than 2021-08-15

## गहराई में जाएं

दो तिथियों को तुलना करने के बारे में अधिक जानने के लिए आप बैश के डेट कम्पेरिजन ऑपरेटर्स को अधिक समझ सकते हैं। ये ऑपरेटर्स तिथियों को सामान्य तारीख प्रारूप में परिवर्तित करते हैं और उन्हें तुलना करते हैं। अधिक जानकारी के लिए, [बैश डॉक्यूमेंटेशन](https://www.gnu.org/software/bash/manual/html_node/Bash-Conditional-Expressions.html) की जांच करें।

## देखें

[बैश डॉक्यूमेंटेशन](https://www.gnu.org/software/bash/manual/html_node/), [बैश ट्यूटोरियल](https://linuxconfig.org/bash-scripting-tutorial), [स्टैक ओवरफ्लो में बैश समझने के लिए फ़्री विडियो सीरीज़](https://overthewire.org/wargames/bandit/)।