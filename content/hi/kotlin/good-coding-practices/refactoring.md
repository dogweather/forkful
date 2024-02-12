---
title:                "कोड की सफाई"
aliases:
- /hi/kotlin/refactoring.md
date:                  2024-01-26T01:43:48.703373-07:00
model:                 gpt-4-0125-preview
simple_title:         "कोड की सफाई"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/refactoring.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
रीफैक्टरिंग एक्सिस्टिंग कोड को ट्वीक करने की प्रक्रिया है ताकि उसकी संरचना, पठनीयता, और प्रदर्शन में सुधार किया जा सके, बिना इसके बाह्य व्यवहार को बदले। प्रोग्रामर कोड को अधिक मेनटेनेबल बनाने के लिए, नई विशेषताओं के जोड़ने को सरल करने के लिए, और बग्स को आसानी से खोजने और ठीक करने के लिए रीफैक्टर करते हैं।

## कैसे करें:
यहाँ एक कोटलिन स्निपेट है जो एक आम कोड स्मेल और उसके रीफैक्टर्ड संस्करण को दिखाता है। हम एक कोड चंक से शुरुआत करते हैं जो बहुत ज्यादा काम कर रहा है:

```kotlin
fun processOrders(orders: List<Order>) {
    for (order in orders) {
        print("Order ID: ${order.id}")
        // कुल ऑर्डर की गणना
        var total = 0.0
        for (item in order.items) {
            total += item.price
        }
        // डिस्काउंट लागू करें
        if (order.customer.isVIP) {
            total *= 0.9
        }
        print("Total: $total")
        // और अधिक प्रोसेसिंग...
    }
}
```

बेहतर पठनीयता और चिंताओं के पृथक्करण के लिए रीफैक्टर्ड:

```kotlin
fun printOrderSummary(order: Order) {
    print("Order ID: ${order.id}")
    val total = calculateTotal(order)
    print("Total: $total")
}

fun calculateTotal(order: Order): Double {
    var total = order.items.sumOf { it.price }
    return if (order.customer.isVIP) total * 0.9 else total
}

fun processOrders(orders: List<Order>) {
    orders.forEach { printOrderSummary(it) }
}
```

यहाँ कोई नमूना आउटपुट नहीं है क्योंकि हमने कार्यक्षमता नहीं बदली, लेकिन कोड की पठनीयता और मेनटेनेबिलिटी में भारी वृद्धि हुई!

## गहन विवेचन
रीफैक्टरिंग की अवधारणा प्रोग्रामिंग शुरू होने के बाद से ही रही है, लेकिन यह 1990 के दशक में, विशेष रूप से मार्टिन फॉउलर ने 1999 में "रिफैक्टरिंग: एक्सिस्टिंग कोड के डिज़ाइन में सुधार" प्रकाशित करने के बाद एक अनुशासन के रूप में वास्तव में उड़ान भरी। इस पुस्तक ने प्रथाओं को एक नाम दिया और इसे लागू करने के लिए एक व्यवस्थित विधि, सहित रिफैक्टरिंग तकनीकों के एक कैटलॉग को परिभाषित किया।

रीफैक्टरिंग की तुलना विकल्पों से: आप कोड को शुरू से लिख सकते हैं (जोखिम भरा और समय लेने वाला), या सिर्फ एडिटिव परिवर्तन कर सकते हैं (सॉफ्टवेयर ब्लोट और संभावित टेक डेट की ओर ले जाता है)। री�
