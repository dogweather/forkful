---
title:                "डीबग आउटपुट प्रिंट करना"
html_title:           "Gleam: डीबग आउटपुट प्रिंट करना"
simple_title:         "डीबग आउटपुट प्रिंट करना"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## क्या और क्यों? 

डीबग लिखना (Print debug output) एक उदाःरण है जिसका उपयोग प्रोग्रामिंग विकासक में बग्स की खोज करने में होता है। डीबग आउटपुट का उपयोग करने से कोड की प्रवृत्ति की समझ सुलभता से समझी जा सकती है और त्रुटियों को ढूंढने में आसानी होती है।

## कैसे :

```Ruby
def debug_print(message)
  if ENV['DEBUG'] == 'true'
    puts "Debug: #{message}"
  end
end

debug_print("Checking debug output in ruby programming.")
```

उदाहरण के तहत, यदि आप 'DEBUG' वातावरण चर से "true" को 'DEBUG' करते हैं, तो "Debug: Checking debug output in ruby programming." को आउटपुट किया जाएगा।

## गहन अध्ययन:

डीबग लिखना (Print debug output) का उपयोग साहित्यिक पाठ्यक्रम में टेस्ट की जांच और कल्पना नियंत्रण के लिए किया जाता है। यह एक पुराना तेक्निक है जिसका उपयोग बग्स खोजने और परिष्करण प्रक्रिया को बेहतर बनाने के लिए किया जाता है। 'puts' और 'p' कमांडों का उपयोग Ruby में डीबग संदेशों की प्रिंट करने के लिए हो सकता है, 'puts' चर मान की स्वचालित रूप से तोड़ता है, जबकि 'p' कमांड मान को देखने के लिए उपयोगी होती है।

## अधिक जानें:


ये संसाधन रूबी प्रोग्रामिंग में डीबग आउटपुट की जानकारी प्रदान करते हैं और आपको आपके प्रोग्राम्स को बेहतर बनाने में मदद करेंगे। उन्हें आखिर तक पढ़ें और उनपर अनुशासन लाएं!