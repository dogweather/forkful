---
title:                "HTML पार्सिंग"
html_title:           "Javascript: HTML पार्सिंग"
simple_title:         "HTML पार्सिंग"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/parsing-html.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
एचटीएमएल की पार्सिंग के बारे में दो या तीन वाक्यों में यह समझाने की कोशिश है कि (1) यह क्या है और (2) प्रोग्रामर्स इसे क्यों करते हैं।

## कैसे करें:
`जावास्क्रिप्ट` के भीतर कुछ कोड ब्लॉक के साथ कोडिंग उदाहरण और नमूना आउटपुट के साथ।

```जावास्क्रिप्ट
// पार्सिंग एक एचटीएमएल फ़ाइल
const fs = require('fs');
const html = fs.readFileSync('index.html', 'utf-8');
console.log(html);
```

आउटपुट:
```
<!DOCTYPE html>
<html>
<head>
	<title>मेरा वेबसाइट</title>
</head>
<body>
	<h1>नमस्ते दुनिया!</h1>
	<p>यह मेरी पहली वेबसाइट है।</p>
</body>
</html>
```

## गहराई में जाएं:
इसमें (1) ऐतिहासिक संदर्भ, (2) वैकल्पिक विकल्प, और (3) पार्सिंग एचटीएमएल के बारे में आयातन विवरण जैसी गहराई में जानकारी है।

## देखें भी:
संबंधित स्रोतों के लिंक।