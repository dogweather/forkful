---
title:                "डायरेक्टरी मौजूद है की नहीं जांचना"
html_title:           "Ruby: डायरेक्टरी मौजूद है की नहीं जांचना"
simple_title:         "डायरेक्टरी मौजूद है की नहीं जांचना"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

##ं क्या है और क्यों?
डायरेक्टरी की मौजूदगी की जांच क्या है और क्यों प्रोग्रामर्स इसे करते हैं? डायरेक्टरी की मौजूदगी की जांच एक प्रोग्रामर के लिए अपने कोड में जांचने का एक तरीका है। यह उन्हें सुनिश्चित करता है कि उनके द्वारा उपयोग किए गए फ़ाइलों और संसाधनों के लिए सही मार्ग पर हैं।

## कैसे:
जब एक प्रोग्रामर अपने कोड में डायरेक्टरी की मौजूदगी की जांच करता है, वे इस कोड का उपयोग करते हैं:
```
if Dir.exist?('directory_name')
    puts "Directory exists!"
else
    puts "Directory does not exist."
end
```
यहां, हमने `Dir.exist?` का उपयोग करके जांचा कि क्या डायरेक्टरी मौजूद है। अगर वह मौजूद है तो हम `Directory exists!` प्रिंट करते हैं, और अगर यह मौजूद नहीं है तो हम `Directory does not exist.` प्रिंट करते हैं।

## गहराई में जाएं:
डायरेक्टरी की मौजूदगी की जांच बहुत ही उपयोगी है, क्योंकि यह आपको सुनिश्चित करता है कि आप अपने कोड में सही संसाधनों का उपयोग कर रहे हैं। इस विषय पर और अधिक जानने के लिए, आप आपके प्रोग्राम को और सुरक्षित करने के लिए अन्य तरीकों के बारे में भी अधिक जान सकते हैं। आप भी `Dir` का डॉक्स को समझ सकते हैं और इस फ़ंक्शन को कैसे इंप्लीमेंट किया गया है इसके बारे में जान सकते हैं।

## इससे संबंधित देखें:
- [Documentation for Dir.exist?](https://ruby-doc.org/core-2.7.0/Dir.html#method-c-exist-3F)
- [Another method for checking directory existence](http://zetcode.com/lang/rubytutorial/conditions/)