---
title:                "नया प्रोजेक्ट शुरू करना"
html_title:           "C: नया प्रोजेक्ट शुरू करना"
simple_title:         "नया प्रोजेक्ट शुरू करना"
programming_language: "Bash"
category:             "Bash"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Bash Programming: एक हिंदी गाइड

## 1. क्या और क्यों?

Nया प्रोजेक्ट शरू करना मतलब हमें नई कोडिंग योजना शुरू करनी होती है। इसका मुख्य कारण होता है कि प्रोग्रामर्स नई आवश्यकताओं को पूरा करने या एक नए सोच को अंजाम देने के लिए यह करते हैं।

## 2. कैसे करें:

एक नया बैश स्क्रिप्ट बनाने के लिए, आपको एक साधारण फ़ाइल बनाने की आवश्यकता है। 

```Bash
touch myscript.sh
```

और इसे शुरू करें `#!/bin/bash` से, यह दर्शाता है कि शेल स्क्रिप्ट बैश में चलाई जा रही है।

```Bash
#!/bin/bash
echo "Hello, World!"
```

फिर आप चला सकते हैं:

```Bash
chmod +x myscript.sh
./myscript.sh
```

आपको "Hello, World!" का उत्तर मिलेगा।

## 3. गहराई से:

### ऐतिहासिक प्रसंग
Bash 1989 में Brian Fox द्वारा GNU project के लिए विकसित किया गया था, और अब यह UNIX/Linux पर सर्वाधिक लोकप्रिय शेल है। 

### विकल्प
अन्य शेल्स जैसे कि Zsh, Fish, Csh, tcsh, Ksh, etc भी होते हैं। प्रत्येक का अपना अद्वितीय उपयोग और लाभ होते हैं।

### क्रियान्वयन विवरण
Bash, सी भाषा में लिखी गई है, और सी प्रिप्रोसेसर का उपयोग करके विभिन्न प्लेटफॉर्मों (Linux, Unix, Windows आदि) पर चलने के लिए अनुकूलित होती है।

## 4. अधिक जानकारी के लिए:

- [GNU Bash Reference Manual](https://www.gnu.org/software/bash/manual/bash.html)
- [Bash Scripting Tutorial for Beginners](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)
- [An in-depth exploration of the art of shell scripting](https://www.tldp.org/LDP/abs/html/)