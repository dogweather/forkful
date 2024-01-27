---
title:                "लॉगिंग"
date:                  2024-01-26T01:10:02.380840-07:00
model:                 gpt-4-1106-preview
simple_title:         "लॉगिंग"
programming_language: "Go"
category:             "Go"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/logging.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
लॉगिंग का मतलब होता है एप्लिकेशन के घटनाओं, स्थितियों और डेटा प्रवाहों का रिकॉर्ड रखना। प्रोग्रामर इसे बग्स का निदान करने, प्रदर्शन का मॉनिटरिंग करने और एप्लिकेशन की ऑपरेशनल स्वास्थ्य की निगरानी करने के लिए करते हैं—यह किसी एयरप्लेन के ब्लैक बॉक्स के सॉफ्टवेयर समकक्ष की तरह होता है।

## कैसे करें:
Go में, लॉगिंग को कई तरीकों से संभाला जा सकता है, जैसे कि स्टैंडर्ड लाइब्रेरी का `log` पैकेज से लेकर थर्ड-पार्टी लाइब्रेरीस जैसे कि `logrus` और `zap` तक। यहाँ एक साधारण उदाहरण है जो बिल्ट-इन `log` पैकेज का उपयोग करता है:

```Go
package main

import (
	"log"
	"os"
)

func main() {
	// एक लॉग फाइल बनाएँ
	logFile, err := os.OpenFile("app.log", os.O_CREATE|os.O_APPEND|os.O_WRONLY, 0666)
	if err != nil {
		log.Fatal(err)
	}
	defer logFile.Close()

	// लॉग आउटपुट को फाइल में सेट करें
	log.SetOutput(logFile)

	// कुछ ईवेंट्स को लॉग करें
	log.Println("Starting the application...")
	// ... यहाँ एप्लिकेशन लॉजिक ...
	log.Println("Application ended successfully.")
}
```

अगर आप इस कोड को चलाएंगे, तो आपको टर्मिनल पर कोई आउटपुट नहीं दिखेगा क्योंकि यह सब `app.log` में जा रहा है। यह लॉग फाइल के अंदर आप जो देखेंगे उसका एक झलक यह है:

```
2023/01/02 15:04:05 Starting the application...
2023/01/02 15:05:01 Application ended successfully.
```

## गहराई में जाना
प्रोग्रामिंग में लॉगिंग की शुरुआत सबसे पहले कंप्यूटर्स के जमाने में हुई थी, जहां इंजीनियर्स वास्तविक बग्स (मॉथ्स, सटीक होने के लिए) को हार्डवेयर में कुचला हुआ पाते थे, और वे उन्हें लॉग करते थे! आज की तारीख में आते हुए, लॉगिंग जटिल सिस्टम्स के भीतर क्या हो रहा है इसे समझने का एक सोफिस्टिकेटेड तरीका बन चुका है।

Go में `log` पैकेज काफी सरल है, लेकिन बेसिक एप्लिकेशनों के लिए यह पर्याप्त हो सकता है। हालाँकि, मॉडर्न वितरित सिस्टम्स के संदर्भ में, या जब आपको अपने लॉग आउटपुट पर अधिक सूक्ष्म नियंत्रण की आवश्यकता हो (जैसे कि गंभीरता के विभिन्न स्तरों), आप अधिक शक्तिशाली समाधान का पता लगाना चाह सकते हैं।

थर्ड-पार्टी लॉगिंग लाइब्रेरीस जैसे कि `logrus` और `zap` स्ट्रक्चर्ड लॉगिंग ऑफर करते हैं, जिसका मतलब है कि आप JSON जैसे जटिल डेटा टाइप्स को लॉग कर सकते हैं, जिससे लॉग्स का विश्लेषण, खासकर लॉग प्रबंधन सिस्टम्स जैसे कि ELK स्टैक या Splunk के साथ मिलकर, आसान हो जाता है।

लॉगिंग रणनीति के कार्यान्वयन को देखते हुए, प्रदर्शन प्रभावों के बारे में सोचना भी अत्यंत महत्वपूर्ण है। हाई-परफॉर्मेंस लॉगिंग लाइब्रेरीस को एप्लिकेशन के थ्रूपुट और लेटेंसी पर प्रभाव को कम करने के लिए अनुकूलित किया जाता है। उदाहरण के लिए, `zap` अपने तेज़, कम आवंटन डिजाइन का दावा करता है, जो रियल-टाइम सिस्टम्स के लिए महत्वपूर्ण हो सकता है।

विभिन्न लाइब्रेरीस के अलावा, लॉगिंग प्रारूप और मानक भी ध्यान देने योग्य हैं। स्ट्रक्चर्ड लॉगिंग प्रारूप जैसे कि JSON, लॉग प्रोसेसिंग सिस्टम्स के संयोजन में इस्तेमाल किए जाने पर अत्यंत शक्तिशाली हो सकते हैं। दूसरी ओर, प्लेन टेक्स्ट लॉग्स मानव पठनीय होते हैं लेकिन प्रोग्रामात्मक रूप से पार्स करने में अधिक चुनौतीपूर्ण होते हैं।

## देखें भी
Go की लॉगिंग क्षमताओं के बारे में और गहराई में जाने के लिए, ये संसाधन उपयोगी हो सकते हैं:

- Go ब्लॉग पर लॉगिंग: https://blog.golang.org/logging
- `logrus`, Go के लिए एक स्ट्रक्चर्ड लॉगर: https://github.com/sirupsen/logrus
- `zap`, एक तेज, स्ट्रक्चर्ड, स्तरित लॉगर: https://github.com/uber-go/zap
- लॉग विश्लेषण के लिए ELK स्टैक (Elasticsearch, Logstash, Kibana): https://www.elastic.co/what-is/elk-stack
- Go लॉगिंग लाइब्रेरीस की तुलना: https://www.loggly.com/blog/benchmarking-5-popular-golang-logging-libraries/