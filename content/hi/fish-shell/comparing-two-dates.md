---
title:                "दो तारीखों की तुलना"
html_title:           "Elixir: दो तारीखों की तुलना"
simple_title:         "दो तारीखों की तुलना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

तारीखों की तुलना करना उन दोनों के बीते समय का तुलनात्मक मूल्यांकन होता है। प्रोग्रामर्स ऐसा करते हैं ताकि वे ज्ञात कर सके कि किसी कार्य को पूरा करने में कितना समय लगा या फिर किसी घटना को कितना समय हो गया।

## कैसे करें:

तारीखों की तुलना की बेहतर समझ के लिए निम्नलिखित Fish Shell कोड देखें:

```fish shell
# तारीख को सेकेंड्स में कनवर्ट करें
function date_to_seconds -a date_string
  date -d $date_string +%s
end

# दो तारीखों की तुलना करें
function compare_dates -a date1 date2 
  set difference (math (date_to_seconds $date1) - (date_to_seconds $date2))
  if test $difference -gt 0
    echo "$date1 is later than $date2"
  else if test $difference -eq 0
    echo "$date1 is the same as $date2"
  else
    echo "$date1 is earlier than $date2"
  end
end
```

## गहरी तलाश:

(1) हिस्टोरिकल कंटेक्स्ट: तारीखों की तुलना ऑपरेटर्स का आविष्कार निर्दिष्ट समय दूरी की आपेक्षिकता को समझने के लिए हुआ था।

(2) विकल्प: Unix परिवार की शेल्स अक्सर तारीखों की तुलना के लिए तरलीकरण का उपयोग करती हैं। फिर भी, कुछ मजबूत भाषाएं (जैसे कि Python और Perl) निर्दिष्ट तारीख और समय की तुलना के लिए विशिष्ट बिल्ट-इन फ़ंक्शन प्रदान करती हैं।

(3) कार्यान्वयन विवरण: तारीखों की तुलना फ़ंक्शन मूल रूप से Unix 'date' उपकरण का उपयोग करता है, जो हमें तारीखों को उनके Unix टाइमस्टैंप के रूप में बदलने की अनुमति देता है।

## देखे भी:

1. Unix 'date' man page: http://man7.org/linux/man-pages/man1/date.1.html
2. Fish shell documentation: https://fishshell.com/docs/current/index.html
3. Comparative Operators in Programming Languages: https://en.wikipedia.org/wiki/Comparison_operator