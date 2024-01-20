---
title:                "डायरेक्टरी मौजूद है या नहीं यह जांचना"
html_title:           "Fish Shell: डायरेक्टरी मौजूद है या नहीं यह जांचना"
simple_title:         "डायरेक्टरी मौजूद है या नहीं यह जांचना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
निर्देशिका का अस्तित्व जांचना मतलब देखना की क्या एक विशेष फ़ाइल या फ़ोल्डर मौजूद है या नहीं। कार्यक्रमकर्ताओं को इसे काम में लाने की जरूरत तब होती है जब उन्हें ऐसे कोड लिखने की जरूरत होती है जो केवल तब ही चले जब एक विशेष निर्देशिका मौजूद हो। 

## कैसे करें:
निम्नलिखित कोड का उपयोग करके आप Fish Shell में निर्देशिका के अस्तित्व की जांच कर सकते हैं।

```Fish Shell
if test -d your_directory
    echo "निर्देशिका मौजूद है"
else
    echo "निर्देशिका उपलब्ध नहीं है"
end
```

यदि 'your_directory' मौजूद होता है, तो यह "निर्देशिका मौजूद है" प्रिंट करेगा, अन्यथा यह "निर्देशिका उपलब्ध नहीं है" प्रिंट करेगा।

## गहराई में:
निर्देशिका की जांच की क्षमता इनिशियल Unix शेल के साथ आती है जिसे 1970 में डेवलप किया गया था। फ़िश शेल ने इसे समर्थन करना जारी रखा है। वैकल्पिक तरीके में, आप ऐसे शब्दों का उपयोग कर सकते हैं जैसे 'find' और 'ls', लेकिन 'test' -d कमांड सबसे सारल और सीधा है। इसे लागू करने के बारे में विस्तृत जानकारी के लिए, आप 'man test' कमांड का उपयोग कर सकते हैं।

## भी देखें:
आप इन संसाधनों का उपयोग करके अपने ज्ञान को बढ़ा सकते हैं

1. Fish Documentation: [दस्तावेज़ीकरण](https://fishshell.com/docs/current/index.html)
2. Unix man pages: [मैन पेज](https://man7.org/linux/man-pages/man1/test.1.html).
3. Shell Scripting Tutorial: [शेल स्क्रिप्टिंग ट्यूटोरियल](https://www.shellscript.sh/).