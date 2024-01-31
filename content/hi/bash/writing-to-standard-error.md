---
title:                "मानक त्रुटि में लिखना"
date:                  2024-01-19
simple_title:         "मानक त्रुटि में लिखना"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
जब हम बात करें स्टैंडर्ड एरर की, तो यह एक स्ट्रीम होता है जहां प्रोग्राम्स अपनी एरर मैसेजेस भेजते हैं। प्रोग्रामर्स इसका इस्तेमाल करते हैं ताकि वे एरर मैसेजेस को सिस्टम के मुख्य आउटपुट से अलग रख सकें, जिससे लॉगिंग और डीबगिंग में आसानी हो।

## कैसे?: (How to)
Bash में, आप 2> रीडायरेक्शन ऑपरेटर का इस्तेमाल करके स्टैंडर्ड एरर को रीडायरेक्ट कर सकते हैं.

```Bash
echo "This is a standard output"
echo "This is a standard error" >&2
```

सैंपल आउटपुट:

```
This is a standard output
This is a standard error
```

आप एरर्स को एक फाइल में भी भेज सकते हैं:

```Bash
echo "This will go to the file" 2> error.log
```

एरर्स को /dev/null में भेजने का उदाहरण, जिससे वे इग्नोर हो जाएंगे:

```Bash
command-with-error 2> /dev/null
```

## गहराई से जानकारी (Deep Dive)
स्टैंडर्ड एरर (stderr) का कांसेप्ट UNIX के प्रारंभिक दिनों से चला आ रहा है। इसे स्टैंडर्ड आउटपुट (stdout) से अलग रखने का मतलब है कि आप नॉर्मल आउटपुट को एक जगह पर और एरर्स को दूसरी जगह पर रीडायरेक्ट कर सकते हैं। ऐसा करने पर, प्रोग्राम्स ज्यादा फ्लेक्सिबल और इजी टू डीबग हो जाते हैं। फाइल डीस्क्रिप्टर 1 (fd 1) आम तौर पर स्टैंडर्ड आउटपुट के लिए रिजर्व होता है, और फाइल डीस्क्रिप्टर 2 (fd 2) स्टैंडर्ड एरर के लिए।

वैकल्पिक तरीकों में, आप `>&2` का इस्तेमाल करके स्टैंडर्ड आउटपुट और एरर दोनों को एक साथ किसी फाइल या डिवाइस में रीडायरेक्ट कर सकते हैं, या उन्हें अलग-अलग हैंडल कर सकते हैं।

## संबंधित जानकारी (See Also)
- Bash Scripting Guide: https://www.gnu.org/software/bash/manual/
- Advanced Bash-Scripting Guide: http://www.tldp.org/LDP/abs/html/
- Bash Redirections Cheat Sheet: https://www.gnu.org/software/bash/manual/html_node/Redirections.html
