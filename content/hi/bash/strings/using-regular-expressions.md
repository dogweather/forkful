---
title:                "रेगुलर एक्सप्रेशन्स का उपयोग करना"
date:                  2024-02-03T19:16:50.081321-07:00
model:                 gpt-4-0125-preview
simple_title:         "रेगुलर एक्सप्रेशन्स का उपयोग करना"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

Bash में नियमित अभिव्यक्तियाँ (regex) आपको विशिष्ट पैटर्न के आधार पर स्ट्रिंग्स और फाइलों को खोजने, मैनिपुलेट करने, और संभालने की अनुमति देती हैं। प्रोग्रामर इनपुट मान्यता, लॉग फाइलों का पार्स करने, और डेटा निष्कर्षण जैसे कामों के लिए regex का उपयोग करते हैं क्योंकि यह जटिल पाठ प्रक्रिया की आवश्यकताओं के लिए पैटर्न निर्दिष्ट करने का एक लचीला और शक्तिशाली तरीका प्रदान करता है।

## कैसे करें:

### मूल पैटर्न मिलान
यदि कोई स्ट्रिंग किसी पैटर्न से मेल खाती है तो पता लगाने के लिए, आप `grep` का उपयोग कर सकते हैं, जो एक कमांड-लाइन यूटिलिटी है जो सादे-पाठ डेटा सेटों में नियमित अभिव्यक्ति से मिलने वाली पंक्तियों की खोज के लिए होती है:

```bash
echo "Hello, World!" | grep -o "World"
# आउटपुट: World
```

### विशिष्ट डेटा निकालना
अपने regex पैटर्न से मेल खाने वाले डेटा के भागों को निकालने के लिए, आप `grep` के साथ `-o` का उपयोग कर सकते है:

```bash
echo "Error: File not found" | grep -oE "[A-Za-z]+:"
# आउटपुट: त्रुटि:
```

### `sed` के साथ Regex का उपयोग करना
`sed` (स्ट्रीम एडिटर) पाठ को पार्स करने और बदलने के लिए एक शक्तिशाली यूटिलिटी है। यहाँ ‘sed’ के साथ regex का उपयोग करके पाठ को बदलने का तरीका है:

```bash
echo "Bash is great" | sed -e 's/great/awesome/'
# आउटपुट: Bash is awesome
```

### सशर्त वक्तव्यों में पैटर्न मिलान
Bash सीधे सशर्त वक्तव्यों में भी regex का समर्थन करता है:

```bash
[[ "https://example.com" =~ ^https?:// ]] && echo "URL is valid" || echo "URL is invalid"
# आउटपुट: URL is valid
```

### `awk` के साथ उन्नत पैटर्न मिलान और संचालन
`awk` एक और पाठ-प्रसंस्करण उपकरण है जो अधिक जटिल डेटा निष्कर्षण और संचालन को समर्थन करता है। यह CSVs जैसे संरचित पाठ डेटा के साथ काम करते समय लाभकारी हो सकता है:

```bash
echo -e "ID,Name,Age\n1,John,22\n2,Jane,24" | awk -F, '$3 > 22 {print $2 " is older than 22."}'
# आउटपुट: Jane is older than 22.
```

जबकि Bash की निर्मित रेजेक्स क्रियाकलापों में कई उपयोग के मामले शामिल हैं, बहुत उन्नत रेजेक्स क्रियाओं के लिए, आप `perl` या `python` स्क्रिप्ट्स के साथ Bash स्क्रिप्ट्स का संयोजन करने पर विचार कर सकते हैं, क्योंकि ये भाषाएँ शक्तिशाली रेजेक्स लाइब्रेरीज (उदाहरण के लिए, Python में `re`) प्रदान करती हैं। Python के साथ एक साधारण उदाहरण:

```bash
echo "Capture this 123" | python3 -c "import sys; import re; print(re.search('(\d+)', sys.stdin.read()).group(0))"
# आउटपुट: 123
```

आवश्यकता होने पर इन प्रोग्रामिंग भाषाओं को शामिल करके, आप अपने Bash स्क्रिप्ट्स में रेजेक्स की पूरी शक्ति का लाभ उठा सकते हैं।