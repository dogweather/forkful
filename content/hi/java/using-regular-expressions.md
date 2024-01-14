---
title:    "Java: नियमित अभिव्यक्ति का उपयोग करना"
keywords: ["Java"]
---

{{< edit_this_page >}}

## क्यों

क्या आप कभी सोचा है कि आप अपने जावा प्रोग्राम को और अधिक शक्तिशाली और दुर्लभ बना सकते हैं? रेगुलर एक्सप्रेशन्स आपको अपने डेटा में बहुत से फॉर्मैटिंग और पैटर्न के साथ खेलने की अनुमति देते हैं। इससे आपको अपने कोड को अधिक सुगठित बनाने के साथ-साथ उसे अधिक रिलेवेंट बनाने का भी मौका मिलता है।

## कैसे करें

आइए अब हम देखें कि हम अपनी जावा प्रोग्राम में रेगुलर एक्सप्रेशन्स का उपयोग कैसे कर सकते हैं। हम एक सरल उदाहरण के साथ शुरू करेंगे।

```Java
// दिए गए स्थानों से सही संख्या ढूंढें
String text = "मेरे गाँव में ४५६ रहनेवाले हैं।";
String pattern = "\\d+";
Pattern r = Pattern.compile(pattern);
Matcher m = r.matcher(text);
if (m.find()) {
	System.out.println("संख्या मिली है: " + m.group(0));
} else {
	System.out.println("संख्या नहीं मिली।");
}
```

उपरोक्त नमूना कोड में हमने दिए गए स्थानों से कोई भी संख्या खोजने का प्रयास किया है और यदि कोई संख्या मिली है तो उसे मैचर के नियमों के अनुसार प्रिंट किया है। इस उदाहरण में हमने खुले आंकड़े का उपयोग किया है लेकिन आप अपनी जरूरत के अनुसार पैटर्न बदल सकते हैं।

## गहराई में जाएं

रेगुलर एक्सप्रेशन्स के साथ काम करने में कुछ दिक्कतें हो सकती हैं, लेकिन अगर आप उन्हें समझने और समाधान करने का सही तरीका जानते हैं तो वे आपके प्रोग्र