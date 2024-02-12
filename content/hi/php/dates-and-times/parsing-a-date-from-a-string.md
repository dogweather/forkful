---
title:                "स्ट्रिंग से तारीख पार्स करना"
date:                  2024-02-03T19:16:12.610342-07:00
model:                 gpt-4-0125-preview
simple_title:         "स्ट्रिंग से तारीख पार्स करना"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

PHP में एक स्ट्रिंग से तारीख पार्स करना एक तारीख और/या समय का प्रतिनिधित्व करने वाले पाठ को PHP `DateTime` ऑब्जेक्ट या अन्य दिनांक/समय प्रारूपों में बदलने की प्रक्रिया है। यह डेटा सत्यापन, हेरफेर, संग्रहण, और प्रस्तुतीकरण उद्देश्यों के लिए महत्वपूर्ण है, विशेष रूप से जब उपयोगकर्ता इनपुट या बाहरी स्रोतों से डेटा के साथ काम करते समय।

## कैसे करें:

PHP की अंतर्निहित `DateTime` क्लास तिथियों के साथ काम करने और उन्हें पार्स करने के लिए एक शक्तिशाली सेट ऑफ फ़ंक्शन प्रदान करती है। आप निर्माणकर्ता का उपयोग करके एक दिनांक स्ट्रिंग से `DateTime` इन्स्टेंस बना सकते हैं, और फिर आवश्यकता अनुसार इसे स्वरूपित कर सकते हैं। आइए देखें कैसे:

```php
$dateString = "2023-04-25 15:30:00";
$dateObject = new DateTime($dateString);

echo $dateObject->format('Y-m-d H:i:s');
// आउटपुट: 2023-04-25 15:30:00
```

जो स्ट्रिंग्स मानक प्रारूपों का अनुसरण नहीं करती हैं, उन्हें संभालने के लिए, आप `createFromFormat` विधि का उपयोग कर सकते हैं, जो आपको इनपुट तिथि के सटीक प्रारूप को निर्दिष्ट करने की अनुमति देती है:

```php
$dateString = "25-04-2023 3:30 PM";
$dateObject = DateTime::createFromFormat('d-m-Y g:i A', $dateString);

echo $dateObject->format('Y-m-d H:i:s');
// आउटपुट: 2023-04-25 15:30:00
```

`DateTime` द्वारा सीधे समर्थित नहीं होने वाले अधिक जटिल पार्सिंग के लिए, PHP `strtotime` फ़ंक्शन प्रदान करता है, जो किसी भी अंग्रेजी पाठ्य दिनांक समय वर्णन को एक यूनिक्स टाइमस्टैम्प में पार्स करने का प्रयास करता है:

```php
$timestamp = strtotime("next Thursday");
echo date('Y-m-d', $timestamp);
// आउटपुट वर्तमान तिथि पर निर्भर करेगा, उदा., "2023-05-04"
```

**तृतीय-पक्ष पुस्तकालयों का उपयोग करना:**

जबकि PHP के अंतर्निहित फ़ंक्शन व्यापक प्रकार के उपयोग के मामलों को कवर करते हैं, कभी-कभी आपको अधिक परिष्कृत पार्सिंग क्षमताओं की आवश्यकता हो सकती है। PHP के DateTime क्लास का विस्तार करने वाली, Carbon लाइब्रेरी, दिनांक/समय हेरफेर के लिए एक समृद्ध सेट की सुविधाएँ प्रदान करती है:

```php
require 'vendor/autoload.php';

use Carbon\Carbon;

$dateString = "Tomorrow";
$date = Carbon::parse($dateString);

echo $date->toDateTimeString();
// आउटपुट परिवर्तित होगा, उदा., "2023-04-26 00:00:00"
```

Carbon की `parse` विधि कई प्रकार के दिनांक और समय प्रारूपों को बुद्धिमानी से संभाल सकती है, जो ऐसे अनुप्रयोगों के लिए एक अमूल्य उपकरण है जिन्हें लचीली दिनांक पार्सिंग कार्यक्षमता की आवश्यकता है।