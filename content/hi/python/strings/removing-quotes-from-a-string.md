---
title:                "स्ट्रिंग से उद्धरण चिह्न हटाना"
date:                  2024-01-26T03:43:16.904117-07:00
model:                 gpt-4-0125-preview
simple_title:         "स्ट्रिंग से उद्धरण चिह्न हटाना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
एक स्ट्रिंग से उद्धरण चिह्नों को हटाना आमतौर पर अतिरिक्त दोहरे (") या एकल (') उद्धरण चिह्नों को हटाने का मतलब होता है। प्रोग्रामर इसे इनपुट को सैनिटाइज़ करने या जब उद्धरण चिह्नों की आगे के प्रसंस्करण के लिए आवश्यकता नहीं होती है, तो करते हैं—जैसे कि टेक्स्ट को एक डेटाबेस में सहेजना या इसे प्रदर्शन के लिए तैयार करना।

## कैसे करें:
पायथन स्ट्रिंग्स से अनवांछित उद्धरण चिह्नों को हटाने के लिए कई तरीके प्रदान करता है। चलिए कुछ उदाहरणों के माध्यम से देखते हैं:

```Python
# उदाहरण 1: str.replace() का उपयोग करके एक उद्धरण के सभी उधाहरणों को हटाना
quote_str = '"Python is awesome!" - Some programmer'
no_quotes = quote_str.replace('"', '')
print(no_quotes)  # आउटपुट: Python is awesome! - Some programmer

# उदाहरण 2: str.strip() का उपयोग करके केवल सिरों से उद्धरण चिह्नों को हटाना
quote_str = "'Python is awesome!'"
no_end_quotes = quote_str.strip("'")
print(no_end_quotes)  # आउटपुट: Python is awesome!

# उदाहरण 3: एकल और दोहरे उद्धरण चिह्नों को संभालना
quote_str = '"Python is \'awesome\'!"'
no_quotes = quote_str.replace('"', '').replace("'", "")
print(no_quotes)  # आउटपुट: Python is awesome!
```

## गहराई में:
उद्धरण चिह्नों को हटाने की प्रक्रिया कम्प्यूटर प्रोग्रामिंग जितनी ही पुरानी है। मूल रूप से, यह सिर्फ डेटा सफाई के बारे में था। जैसे-जैसे सिस्टम विकसित हुए और UI, सर्वर, और डेटाबेस जैसी विभिन्न परतों के माध्यम से बातचीत करना शुरू किया—स्ट्रिंग्स को साफ करना गलतियों या सुरक्षा समस्याओं को रोकने के लिए महत्वपूर्ण बन गया। उदाहरण के लिए, SQL इंजेक्शन को डेटा को एक डेटाबेस में डालने से पहले उपयोगकर्ता के इनपुट्स में उद्धरण चिह्नों को हटाकर या एस्केप करके कम किया जा सकता है।

ऊपर दिखाए गए तरीकों के विकल्पों में नियमित अभिव्यक्तियां शामिल हैं, जो साधारण उद्धरण हटाने के लिए ओवरकिल हो सकती हैं लेकिन सोफ़िस्टिकेटेड पैटर्न मैचिंग के लिए शक्तिशाली होती हैं। उदाहरण के लिए, `re.sub(r"[\"']", "", quote_str)` से एकल या दोहरे उद्धरण चिह्नों के सभी उदाहरणों को एक खाली स्ट्रिंग के साथ प्रतिस्थापित किया जा सकता है।

उद्धरण हटाने को लागू करते समय, याद रखें कि संदर्भ मायने रखता है। कभी-कभी आपको स्ट्रिंग के भीतर के उद्धरण चिह्नों को संरक्षित करने की आवश्यकता होती है लेकिन सिरों के उद्धरणों को हटाना होता है, इसलिए `strip()`, `rstrip()` या `lstrip()` आपके मित्र हैं। दूसरी ओर, यदि आपको सभी उद्धरणों को हटाने की आवश्यकता है या `&quot;` जैसे एन्कोडेड उद्धरणों को संभालना है, तो आप शायद `replace()` का उपयोग करेंगे।

## देखें भी:
- [पायथन स्ट्रिंग दस्तावेज़ीकरण](https://docs.python.org/3/library/string.html)
- [पायथन नियमित अभिव्यक्तियाँ (re मॉड्यूल)](https://docs.python.org/3/library/re.html)
- [SQL इंजेक्शन को रोकने पर OWASP मार्गदर्शिका](https://owasp.org/www-community/attacks/SQL_Injection)