---
title:    "Gleam: टेक्स्ट खोज और प्रतिस्थापन करना"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## क्यों

शब्दों के खोज और बदलाव को आसान बनाने के लिए त्वरित, स्पष्ट और स्वतंत्र समाधान की तलाश एक सामान्य कौशल है। यह एक खुशहाल डेवलपर के लिए महत्वपूर्ण है क्योंकि यह समस्याओं को आसानी से समाधान करने में मदद करता है और समय भी बचाता है।

## कैसे करें

```
Gleam के अंदर मैक्रो `replace` को उपयोग करें।
```


यह मैक्रो स्ट्रिंगिंग अपरेशन्स की तुलना में स्पष्टता, शक्ति और लोकप्रियता का एक दर्जा प्रदान करता है। नीचे दिए गए उदाहरण को देखें।
 
```
fn main() {
  let text = "सुबह";
  let replaced_text = Gleam.string.replace(text, "ह", "न");
  assert(replaced_text == "सुबनह");
}
```

## गहराई में जाएं

जब हम शब्दों को खोजने और बदलने की बात करते हैं, तो हमें प्रत्येक समस्या के लिए अनुकूलित तरीकों की आवश्यकता होती है। गहराई में जाने से पहले, हमें स्पष्ट होने की आवश्यकता होती है कि हमें कौन सा फंक्शन और उपकरण का इस्तेमाल करना है और इसमें क्या विशेषताएं हैं।

ग्लीम में `string.replace` फंक्शन वास्तव में शब्दों को खोजने और बदलने के लिए अद्भुत गुणवत्ता का एक स्थायी समाधान है। हम यह फंक्शन `pattern`, `replacement` और `text` पैरामीटर के साथ बुलाते हैं और इसे फॉर्मेटिंग, टेक्स्ट संशोधन और अन्य योग्यताओं के साथ उपयोग कर सकते हैं।
 
## देखें

ग्लीम में शब्दों को खोजने और ब