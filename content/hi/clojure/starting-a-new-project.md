---
title:                "नए प्रोजेक्ट की शुरुआत"
html_title:           "Clojure: नए प्रोजेक्ट की शुरुआत"
simple_title:         "नए प्रोजेक्ट की शुरुआत"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/starting-a-new-project.md"
---

{{< edit_this_page >}}

## क्यों

अगर आप सालों से कोडिंग का जादू सीख रहे हैं, आपको शायद अब एक एक नया प्रोजेक्ट शुरू करने के लिए तैयार हो गए हैं। शायद आपको अपनी नौकरी से इनकम पर संतोष नहीं है या आपने एक आवेदन खरोंचा है और एक अपने बच्चों को बेहतर भविष्य देने की उम्मीद का सपना सुनिश्चित किया है। चलिए आज हम इसके लिए यहां एक नया प्रोजेक्ट शुरू करने के तरीके के बारे में बात करें।

## कैसे करें

अगर आप Clojure में नए प्रोजेक्ट को शुरू करना चाहते हैं, तो आपको निम्न चरणों का पालन करना होगा:

```Clojure
(defproject new-project "0.1.0-SNAPSHOT"
  :description "A new Clojure project."
  :dependencies [[org.clojure/clojure "1.10.1"]]
  :main ^:skip-aot new-project.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}
             :dev {:source-paths ["dev"]}
             :test {:source-paths ["test"]}})
```

जैसा कि आप देख सकते हैं, हमने `new-project` नाम का एक प्रोजेक्ट बनाया है जो `0.1.0-SNAPSHOT` संस्करण पर काम करता है। `:dependencies` की सहायता से हमने पंजीकृत ग्रुप और उनके संस्करण के बीच से हमें जिस ग्रुप को आप प्रोजेक्ट के लिए चाहते हैं, को कैसे देखना है। उबेर जार (uberjar) एक जार है जो सभी आवश्यक फाइलों को इकट्ठा करके आपके प्रोजेक्ट को एकदम सही रूप में दर्शाता है। आप इसे उबेर-जार जार (uber-jar.jar) में बना सकते हैं। 

## गहराई तक

अब जब आपने नए प्रोजेक्ट को सफलतापूर्वक शुरू कर लिया है, आपको इसे बन