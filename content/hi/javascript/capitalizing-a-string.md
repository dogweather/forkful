---
title:                "स्ट्रिंग को कैपिटलाइज़ करना"
date:                  2024-01-19
html_title:           "C: स्ट्रिंग को कैपिटलाइज़ करना"
simple_title:         "स्ट्रिंग को कैपिटलाइज़ करना"

category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
Capitalizing a string (स्ट्रिंग को कैपिटलाइज़ करना) मतलब हर शब्द के पहले अक्षर को बड़ा (Uppercase) करना है। Programmers इसे टाइटल्स, हेडिंग्स, और यूज़र के नाम जैसे जगहों पर प्रयोग करते हैं, जिससे पाठ को औपचारिक और सुस्पष्ट रूप दिया जा सके।

## How to: (कैसे करें:)
```javascript
function capitalizeString(str) {
  return str.split(' ').map(word => word.charAt(0).toUpperCase() + word.slice(1)).join(' ');
}

console.log(capitalizeString('नमस्ते दुनिया')); // Outputs: नमस्ते दुनिया
console.log(capitalizeString('javascript सीखना आसान है')); // Outputs: Javascript सीखना आसान है
```

## Deep Dive (गहराई से विचार):
Capitalizing की तकनीक पुरानी है, टाइपराइटर के जमाने से है। CSS में `text-transform: capitalize;` जैसा property है, लेकिन जावास्क्रिप्ट में ऐसा built-in फंक्शन नहीं है, इसीलिए हमे function खुद बनाना पड़ता है। वैकल्पिक रूप से, lodash की `_.capitalize` जैसी libraries भी इस्तेमाल हो सकती हैं। जब हम ऊपर दिखाए गए `capitalizeString` function को इम्प्लीमेंट करते हैं, तो `split` से string को words में बांटते हैं, फिर `map` से हर word के पहले character को uppercase में बदलते हैं, और `join` से उन्हें वापस string में जोड़ते हैं।

## See Also (इसे भी देखें):
- MDN Web Docs जावास्क्रिप्ट String Methods: [MDN web docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String)
- Lodash Documentation for Capitalize: [Lodash](https://lodash.com/docs/#capitalize)
