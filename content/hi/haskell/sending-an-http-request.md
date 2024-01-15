---
title:                "एक http अनुरोध भेजना"
html_title:           "Haskell: एक http अनुरोध भेजना"
simple_title:         "एक http अनुरोध भेजना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## इसलिए

एचटीटीपी अनुरोध भेजने में क्यों लगता है, इसका कारण यह है कि हम अपनी एपीआई और उसके साथ संवाद करने के लिए इसका उपयोग करते हैं, जो हमारे दैनिक कार्य में बहुत उपयोगी हो सकता है।

## कैसे

```Haskell
import Network.HTTP

main :: IO ()
main = do
    -- यहां हमें एचटीटीपी अनुरोध को बनाने के लिए आवश्यक विवरण प्रदान करने की आवश्यकता होती है
    -- इस उदाहरण में, हम Google वेबसाइट से डेटा लेने के लिए अनुरोध भेज रहे हैं
    resp <- simpleHTTP (getRequest "http://www.google.com")
    -- अनुरोध के उत्तर को प्राप्त करने के लिए, हम उसे बाइट स्ट्रीम के रूप में पढ़ने के लिए ResponseBody का उपयोग कर सकते हैं
    body <- getResponseBody resp
    -- उत्तर के साथ कुछ छपाई जाएगा
    print body
```

उपरोक्त कोड का नतीजा निम्नलिखित हो सकता है:

```Haskell
<!doctype html> 
<html itemscope="" itemtype="http://schema.org/WebPage" lang="en"> 
<head><meta charset="UTF-8"><meta content="origin" name="referrer"><meta content="IE=Edge" http-equiv="X-UA-Compatible"><meta content="text/html; charset=utf-8" http-equiv="Content-Type"><meta content="width=device-width,initial-scale=1" name="viewport"><meta content="telephone=no" name="format-detection"><meta content="address=no" name="format-detection"><meta content="index,follow" name="robots"><meta content="follow,noindex" name="googlebot">...
```

## गहराई में जाएं

एचटीटीपी या Hyper Text Transfer Protocol, इंटरनेट प्रोटोकॉल का एक हिस्सा है, जो वेब के हमारे दैनिक कार्य को सख्त रखता है। यह ऊपर वर्णित उदाहरण से समझ पाना मुश्किल हो सकता है, लेकिन असल में वह एक अत्यंत प्रभावी और उपयोगी उदाहरण है। आप अन्य वेबसाइटों से भी डेटा लेने या अपनी एपीआई को दूसरों को उप