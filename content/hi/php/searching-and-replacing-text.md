---
title:    "PHP: टेक्स्ट को खोजना और परिवर्तन करना"
keywords: ["PHP"]
---

{{< edit_this_page >}}

# क्यों

टेक्स्ट को खोजने और बदलने की प्रक्रिया में शामिल होने का क्या कारण है? आपको अपने प्रोजेक्ट में भाषा या डेटा के नए रूप को जल्दी से बदलना है तो यह आपके लिए बहुत उपयोगी साबित हो सकता है।

# कैसे

टेक्स्ट को खोजने और बदलने के लिए कौन से फंक्शन का इस्तेमाल करना चाहिए? नीचे दिए गए कुछ सरल PHP कोड ब्लॉक में हम आपको नेस्टेड और संवेदनशील कॉरेक्शन के बारेमें बताएंगे जिससे आप टेक्स्ट को आसानी से बदल सकेंगे:

```
// नेस्टेड कॉरेक्शन
str_replace("आ", "ई", "आपका नाम क्या है?"); // ईपका नाम क्या है?

// संवेदनशील कॉरेक्शन
str_ireplace("आ", "ई", "क्या आपको आपका नाम चेंज कर देना है?"); // क्या ईपको ईपका नाम चेंज कर देना है?
```

# डीप डाइव

टेक्स्ट को खोजने और बदलने के लिए पीएचपी के साथ कैसे आप क्षमताओं को और बढ़ाएंगे? निम्नलिखित लिंक में हमें टेक्स्ट को खोजने और बदलने के लिए क्षमताओं के बारे में और गहराई से जानने के लिए जानकारी उपलब्ध है:

- [PHP.net मैनुअल: str_replace()](https://www.php.net/manual/en/function.str-replace.php)
- [PHP.net मैनुअल: str_ireplace()](https://www.php.net/manual/en/function.str-ireplace.php)
- [PHP: Hypertext Preprocessor - W3Schools](https://www.w3schools.com/php/) 

# आगामी साधन

अगर आपको टेक्स्ट को खोजने और बदलने के बारे में और गंभीर जानकारी चाहिए तो इन लिंक्स की जांच करें:

- [Techopedia - टेक्स्ट को खोजने