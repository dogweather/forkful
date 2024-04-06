---
date: 2024-01-20 17:50:55.903408-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902) Bash \u092E\
  \u0947\u0902 string interpolation \u0915\u0930\u0928\u093E \u0906\u0938\u093E\u0928\
  \ \u0939\u0948\u0964 \u092F\u0939\u093E\u0902 \u0915\u0941\u091B \u0909\u0926\u093E\
  \u0939\u0930\u0923 \u0939\u0948\u0902."
lastmod: '2024-04-05T21:53:54.576157-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902) Bash \u092E\u0947\u0902\
  \ string interpolation \u0915\u0930\u0928\u093E \u0906\u0938\u093E\u0928 \u0939\u0948\
  \u0964 \u092F\u0939\u093E\u0902 \u0915\u0941\u091B \u0909\u0926\u093E\u0939\u0930\
  \u0923 \u0939\u0948\u0902."
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0907\u0902\u091F\u0930\u092A\
  \u094B\u0932\u0947\u0936\u0928"
weight: 8
---

## How to: (कैसे करें)
Bash में string interpolation करना आसान है। यहां कुछ उदाहरण हैं:

```Bash
# Variable Interpolation
name="अनिल"
welcome_message="नमस्ते, $name!"
echo $welcome_message  # ये "नमस्ते, अनिल!" प्रिंट करेगा।

# Command Substitution
user_count=$(who | wc -l)
echo "लॉग्ड इन यूजर्स की संख्या: $user_count"

# Arithmetic Interpolation
a=5
b=2
result=$((a+b))
echo "परिणाम: $result" # ये "परिणाम: 7" प्रिंट करेगा।
```

## Deep Dive (गहराई में)
String interpolation शायद एक नहीं बल्कि सबसे पुरानी programming languages की शुरुआत से ही मौजूद है। Bash में इसका इस्तेमाल काफी सहज होता है - यह सिर्फ वैरिएबल को डबल quotes ("") के अंदर रखने की बात है। लेकिन, इसमें कुछ precautions भी हैं, जैसे कि स्ट्रिंग में स्पेशल कैरेक्टर्स हों तो उन्हें escape करना।

Bash के अल्टर्नेटिव्स जैसे कि Perl या Python अपने अपने तरीकों से interpolation करते हैं। Bash आम तौर पर command substitution के लिए backticks (`) का इस्तेमाल करता था, लेकिन modern scripts में इसकी जगह $(...) का प्रयोग ज्यादा होता है क्योंकि यह नेस्टेड commands को सपोर्ट करता है।

Arithmetic interpolation के लिए Bash डबल parenthesis ((...)) का उपयोग करता है। यह न सिर्फ वैरिएबल को रिप्लेस करता है बल्कि arithmetic operations को भी केलक्यूलेट करता है।

## See Also (अधिक जानकारी के लिए)
- [Bash String Manipulation Guide](https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion)
- [Advanced Bash-Scripting Guide](http://tldp.org/LDP/abs/html/)

इन लिंक्स पर क्लिक करके आप Bash programming में string interpolation के बारे में और भी गहराई से जानकारी प्राप्त कर सकते हैं।
