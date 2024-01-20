---
title:                "http अनुरोध भेजना"
html_title:           "Elixir: http अनुरोध भेजना"
simple_title:         "http अनुरोध भेजना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

# HTTP Anurodh भेजना कैसे सीखें ? 

## क्या और क्यों?

HTTP अनुरोध एक पुनर्वार्ती प्रक्रिया है, जिसके द्वारा कंप्यूटर कोड अन्य कंप्यूटर से जानकारी मांगता है। प्रोग्रामर इसे तब करते हैं जब वे इंटरनेट से डेटा संग्रहित करना चाहते हैं।

## कैसे:

Haskell में HTTP अनुरोध भेजने के लिए 'http-conduit' लाइब्रेरी का उपयोग करें। निम्नलिखित कोड उदाहरण देखें:

```Haskell
import Network.HTTP.Simple

main :: IO ()
main = do
    response <- httpBS "http://httpbin.org/get"
    print $ getResponseBody response
```

उपरोक्त कोड एक HTTP GET अनुरोध भेजता है, और प्रतिक्रिया को प्रिंट करता है।

## गहरे प्रवेश:

1. **ऐतिहासिक प्रसंग:** HTTP अनुरोध का उपयोग 1990 के दशक में WWW (वर्ल्ड वाइड वेब) की शुरुआत के समय से किया जा रहा है।

2. **विकल्प:** 'http-client', 'wreq', और 'req' जैसे अन्य Haskell पैकेज भी होते हैं जो HTTP अनुरोधों के लिए उपयोग किए जा सकते हैं।

3. **कार्यान्वयन विवरण:** 'http-conduit' पैकेज एक हाई लेवल एपीआई प्रदान करता है जो हास्केल के 'सेलेक्ट राइट' के साथ काम करता है, जो एक सॉकेट पर ब्लॉकिंग I/O करने की अनुमति देता है।

## यह भी देखें :

1. [http-conduit लाइब्रेरी](https://hackage.haskell.org/package/http-conduit)
2. [Haskell HTTP पैकेज](https://hackage.haskell.org/package/HTTP)

आशा है कि यह लेख हास्केल प्रोग्रामिंग भाषा में HTTP अनुरोध भेजने में सहायता करेगा। आप लेख से सम्बंधित किसी भी प्रश्न के लिए मुझसे संपर्क कर सकते हैं।