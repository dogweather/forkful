---
title:                "कोड संशोधन"
aliases:
- /hi/php/refactoring/
date:                  2024-01-26T01:54:15.109007-07:00
model:                 gpt-4-0125-preview
simple_title:         "कोड संशोधन"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/refactoring.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
रीफैक्टरिंग एक प्रक्रिया है जिसमें मौजूदा कंप्यूटर कोड की संरचना में परिवर्तन किया जाता है बिना उसके बाहरी व्यवहार में परिवर्तन किए। प्रोग्रामर्स सॉफ्टवेयर के गैर-कार्यात्मक गुणों में सुधार के लिए रीफैक्टरिंग करते हैं, कोड को साफ़, अधिक कुशल और रख-रखाव में आसान बनाते हैं।

## कैसे:
एक क्लासिक PHP स्निपेट को लेते हैं और उसपर कुछ रीफैक्टरिंग जादू लागू करते हैं।

रीफैक्टरिंग से पहले, हमारा कोड ऐसा दिख सकता है:

```php
function printOrderDetails($order) {
    foreach ($order as $item) {
        echo "Item: " . $item['name'];
        echo " - Price: " . $item['price'];
        echo "<br>";
    }
    
    if (!empty($order)) {
        echo "Total: " . array_sum(array_column($order, 'price'));
    }
}
```

लेकिन हम इस कोड को इसकी स्पष्टता और मॉड्यूलरिटी में सुधार के लिए रीफैक्टर कर सकते हैं:

```php
function printItem($item) {
    echo "Item: {$item['name']} - Price: {$item['price']}<br>";
}

function calculateTotal($order) {
    return array_sum(array_column($order, 'price'));
}

function printOrderDetails(array $order) {
    array_walk($order, 'printItem');

    if (!empty($order)) {
        echo "Total: " . calculateTotal($order);
    }
}
```
`printOrderDetails` फ़ंक्शन को छोटे फ़ंक्शनों में विभाजित करके, हमारा कोड अधिक पठनीय और डीबग करने में आसान बन जाता है।

## गहराई से जानकारी
रीफैक्टरिंग की जड़ें शुरुआती 1990 के दशक के स्मॉलटॉक प्रोग्रामिंग समुदाय में हैं और मार्टिन फाउलर की अग्रणी पुस्तक "Refactoring: Improving the Design of Existing Code" (1999) द्वारा और लोकप्रिय किया गया। हालांकि रीफैक्टरिंग को किसी भी प्रोग्रामिंग भाषा में लागू किया जा सकता है, PHP की डायनेमिक प्रकृति कुछ अनूठी चुनौतियों और अवसरों को प्रस्तुत करती है।

रीफैक्टरिंग के विकल्प में कोड को स्क्रैच से फिर से लिखना शामिल हो सकता है, जो अक्सर जोखिम भरा और समय लेने वाला होता है। PHP इकोसिस्टम में, PHPStan और Rector जैसे उपकरण क्रमशः कुछ रीफैक्टरिंग ऑपरेशनों को स्वचालित रूप से स्पॉट और प्रदर्शन कर सकते हैं। कार्यान्वयन के दृष्टिकोण से, रीफैक्टरिंग्स को छोटा रखना और यूनिट टेस्ट्स के साथ व्यापक रूप से परीक्षण करना कीड़े न लाने की सुनिश्चितता में महत्वपूर्ण प्रथाएं हैं।

## देखें भी
- मार्टिन फाउलर की रिफैक्टरिंग पुस्तक: https://martinfowler.com/books/refactoring.html
- PHPStan, एक PHP स्टेटिक विश्लेषण उपकरण: https://phpstan.org/
- Rector, PHP कोड की ऑटोमेटिक रीफैक्टरिंग के लिए एक उपकरण: https://getrector.org/
- PHP यूनिट टेस्टिंग के साथ PHPUnit: https://phpunit.de/
