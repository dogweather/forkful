---
title:                "जटिल संख्याओं के साथ काम करना"
date:                  2024-02-03T18:15:04.590510-07:00
model:                 gpt-4-0125-preview
simple_title:         "जटिल संख्याओं के साथ काम करना"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/working-with-complex-numbers.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

जटिल संख्याएँ एक वास्तविक भाग और एक काल्पनिक भाग से मिलकर बनती हैं, जिन्हें `a + bi` के रूप में दर्शाया जाता है जहाँ `i` का मतलब `-1` का वर्गमूल है। प्रोग्रामर विभिन्न क्षेत्रों जैसे कि विद्युत अभियांत्रिकी, क्वांटम कम्प्यूटिंग, और द्रव गतिकी में जटिल संख्याओं के साथ काम करते हैं, उनकी अनूठी संपत्तियों का उपयोग सिमुलेशन, सिग्नल प्रोसेसिंग, और विशिष्ट प्रकार के गणितीय समीकरणों को हल करने के लिए करते हैं।

## कैसे करें:

C में, जटिल संख्याओं का समर्थन स्टैंडर्ड लाइब्रेरी, विशेष रूप से `<complex.h>` द्वारा किया जाता है। उनका उपयोग करने के लिए, `double complex` प्रकार (या सिंगल प्रेसिजन के लिए `float complex`) के साथ वेरिएबल्स को डिक्लेयर करें। यहाँ बेसिक ऑपरेशंस कैसे करें इसका उदाहरण है:

```c
#include <stdio.h>
#include <complex.h>

int main() {
    double complex z1 = 1.0 + 2.0*I; // एक जटिल संख्या 1+2i घोषित करें
    double complex z2 = 1.0 - 2.0*I; // एक और जटिल संख्या 1-2i घोषित करें
    
    // जोड़
    double complex sum = z1 + z2;
    printf("Sum: %.2f + %.2fi\n", creal(sum), cimag(sum)); // आउटपुट: Sum: 2.00 + 0.00i

    // गुणन
    double complex product = z1 * z2;
    printf("Product: %.2f + %.2fi\n", creal(product), cimag(product)); // आउटपुट: Product: 5.00 + 0.00i

    // जटिल संयुग्म
    double complex conjugate = conj(z1);
    printf("Conjugate of z1: %.2f + %.2fi\n", creal(conjugate), cimag(conjugate)); // आउटपुट: Conjugate of z1: 1.00 - 2.00i
    
    // मैग्नीट्यूड
    double magnitude = cabs(z1);
    printf("Magnitude of z1: %.2f\n", magnitude); // आउटपुट: Magnitude of z1: 2.24

    // चरण
    double phase = carg(z1);
    printf("Phase of z1: %.2f\n", phase); // रेडियन में आउटपुट
    
    return 0;
}
```
नोट करें कि `I` `<complex.h>` में काल्पनिक इकाई को दर्शाने वाला एक स्थिरांक है। `creal()` और `cimag()` जैसे फंक्शन क्रमशः वास्तविक और काल्पनिक भागों को निकालते हैं, जबकि `conj()` जटिल संयुग्म की गणना करता है। जटिल संख्याओं के मैग्निट्यूड और फेज (आर्ग्युमेंट) के लिए, `cabs()` और `carg()` का उपयोग किया जाता है।

## गहराई में जानकारी

C में जटिल संख्याओं का समर्थन अपेक्षाकृत हाल ही में हुआ है, जिसे C99 में मानकीकृत किया गया था। इससे पहले, C में जटिल संख्या अंकगणित करना जटिल था, अक्सर इसके लिए कस्टम डेटा संरचनाओं और फंक्शन्स की आवश्यकता होती थी। `<complex.h>` और जटिल डेटा प्रकारों का समावेश वैज्ञानिक और इंजीनियरिंग अनुप्रयोगों के लिए भाषा की क्षमताओं को महत्वपूर्ण रूप से बढ़ा दिया। हालांकि, यह ध्यान देने योग्य है कि कुछ भाषाएँ, जैसे कि पायथन, जटिल संख्याओं के लिए अधिक सहज समर्थन प्रदान करती हैं, बिल्ट-इन डेटा प्रकारों और एक समृद्ध सेट के लाइब्रेरी फंक्शन्स के माध्यम से। इसके बावजूद, उच्च प्रदर्शन कम्प्यूटिंग कार्यों के लिए सी द्वारा प्रदान किया गया प्रदर्शन और नियंत्रण इसे एक पसंदीदा विकल्प बनाता है, भले ही इसका मतलब जटिल अंकगणित के लिए थोड़ा अधिक वर्बोज सिंटैक्स के साथ डील करना हो।