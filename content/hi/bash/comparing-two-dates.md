---
title:                "दो तारीखों की तुलना"
html_title:           "Elixir: दो तारीखों की तुलना"
simple_title:         "दो तारीखों की तुलना"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
दो तारीखों की तुलना, एक प्रोग्रामिंग क्रिया है जिसमें हम तारीखों के बीच का समय अंतर निर्धारित करते हैं। यह कार्य सुनिश्चित करने के लिए आवश्यक होता है की किसी निशित शर्त को पूरा किया गया है या नहीं, तारीखों को क्रमबद्ध करने के लिए, अथवा समय-सम्बंधित डाटा को व्यवस्थित करने के लिए।

## कैसे करें:
कोड उदाहरण और सैंपल आउटपुट:
```Bash
#!/bin/bash
date1=$(date -d "2022-02-05" +%s)
date2=$(date -d "2023-02-05" +%s)

if [ $date1 -eq $date2 ]
then 
  echo "तारीखें समान हैं।"
elif [ $date1 -gt $date2 ]
then 
  echo "पहली तारीख दूसरी तारीख से बड़ी है।"
else
   echo "पहली तारीख दूसरी तारीख से छोटी है।"
fi
```
## गहरी डाइव:
Bash प्रोग्रामिंग का इतिहास तारीखों के तुलना की क्षमता पर टिका हुआ है। Bash (शॉर्ट फॉर बॉरन अगेन शेल) का विकास 1989 में Brian Fox द्वारा किया गया था, UNIX के original शेल के एक स्वतंत्र विकल्प के रूप में। बचत की गणना, समय की गणना, और डेडलाइन प्रबंधन के लिए तारीखें महत्वपूर्ण होती हैं। विकल्पों में Perl, Python, और Ruby जैसी भाषाएं शामिल हैं, जो तारीखों को और अधिक तेज़ी से और अभिव्यक्तिपूर्णता के साथ संभाल सकती हैं।

## और देखें:
1. `date` कमांड: https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
2. अतिरिक्त प्रोग्रामिंग टूल्स जैसे Perl, Python, और Ruby.
3. Bash प्रोग्रामिंग का गाइड: https://tldp.org/HOWTO/Bash-Prog-Intro-HOWTO.html
4. Bash में Date और Time का प्रबंधन: https://linuxize.com/post/bash-time-date/